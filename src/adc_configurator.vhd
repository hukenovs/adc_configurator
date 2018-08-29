-------------------------------------------------------------------------------
--
-- Title		: adc_configurator.vhd
-- Author		: Alexander Kapitanov
-- Company		: ...
-- E-mail		: sallador@bk.ru
--
-- Version		: 1.0	
--
-------------------------------------------------------------------------------
--
-- Description :  Прием данных с субмодуля
--				  -	до 16 бит, LVDS/LVTTL/LVPECL, 
--				  - DDR/SDR, снижение битрейта в 2/4/8 раз (см. SERDES)
--				  - при некратной разрядности выравнивание данных по старшему биту
--				  -	отдельные каналы для сигналов старта и переполнения
--				  -	в режиме DDR используется SERDES_WIDTH = x1	 
--				  - в режиме SDR используется SERDES_WIDTH > 0	 
--				  -	Использование дифференциальных линий данных и такта	
--			 
-------------------------------------------------------------------------------
--
--  Version 1.0   13.08.2015
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package adc_configurator_pkg is
	constant NCHAN				: integer:=4;	--! Количество независимых каналов АЦП
	constant DATA_WIDTH			: integer:=1;	--! разрядность данных АЦП
	constant FIFO_ADDR			: integer:=12;	--! Глубина FIFO после перепаковщика DEPTH = 2**ADDR	
		
	constant DATA_CONFIG		: integer:=1; --! Режим приёма данных: 1 - SDR / 2- DDR (для SERDES_WIDTH = x1)	
	constant SERDES_WIDTH		: natural:=8; --! Разрядность SERDES линии х1, х2, х4, х8 (х1 - если SERDES не используется)	
	type std_logic_array_NxM is array (NCHAN-1 downto 0) of std_logic_vector(DATA_WIDTH-1 downto 0);	
	type std_logic_array_Nx1 is array (NCHAN-1 downto 0) of std_logic;	
	type std_logic_array_Nx5 is array (NCHAN-1 downto 0) of std_logic_vector(4 downto 0);	
	type std_logic_array_NxK is array (NCHAN-1 downto 0) of std_logic_vector(DATA_CONFIG*SERDES_WIDTH-1 downto 0);	
	
	type std_logic_array_Wx5 is array (NCHAN-1 downto 0) of std_logic_vector(5*DATA_WIDTH-1 downto 0);
    
	constant PACK_OUT			: integer:=64;	--! Ширина шины на выходе перепаковщика: 64, 128, 256, 512	
	constant CLK_SEL			: integer:=0;	--! Выбранный источник тактовой частоты от многоканальной системы ( < или = NCHAN)	
	
	function log2ceil(arg : positive) return natural;
	
end package;
package body adc_configurator_pkg is

	function log2ceil(arg : positive) return natural is
		variable tmp : positive     := 1;
		variable log : natural      := 0;
	begin
		if arg = 1 then return 1; end if; -- careful! return 0 is right for log func!
		while arg > tmp loop
			tmp := tmp * 2;
			log := log + 1;
		end loop;
		return log;
	end function;
	
end adc_configurator_pkg;

library ieee;
use ieee.std_logic_1164.all;	   
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

library unisim;
use unisim.vcomponents.all;

library work;
use work.adc_configurator_pkg.all;
use work.adc_receiver_pkg.all;
use work.ctrl_repackNxM_pkg.all;
use work.ctrl_fifo_config_pkg.all;
use work.ctrl_spi_adc_pkg.all;

entity adc_configurator is	   
	generic (	
		USE_MMCM				: boolean	:=FALSE; --! Использование MMCM вместо узла BUFR
		
		DIFF_TERM				: boolean	:=TRUE; --! Использование дифференциальных сигналов (TRUE/FALSE)
		IOSTANDARD				: string 	:="LVDS_25"; --! Стандарт линий для LVDS
		DATA_IOBDELAY_TYPE		: string 	:="VARIABLE"; --! Режим работы узла задержки IODELAY
		DATA_IOBDELAY_VALUE		: integer 	:=13; --! Значение задержки в режиме "FIXED"
		
		DIFF_CLOCK				: boolean	:=TRUE; --! Дифференциальный тактовый сигнал (TRUE/FALSE)
		
		OVR_PRES				: string 	:="YES"; --! Использование линий переполнения 
		STR_PRES				: string 	:="YES"; --! Использование линий старта
		RESYNC_OUT				: string 	:="YES" --! Использование механизма пересинхронизации выходов через FIFO (NO -  регистр)
		--INV_MASK				: std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'0')  -- маска инверсии по входу
	);
	port (
		-- Common Interface ----
		reset					: in  std_logic;		--! 0 - Global reset		   
		---- ADC Interface ---- 
		adc_p					: in  std_logic_array_NxM; --! Data input P
		adc_n					: in  std_logic_array_NxM; --! Data input N 
				
		ovr_p					: in  std_logic_array_Nx1; --! Overflow P 
		ovr_n 					: in  std_logic_array_Nx1; --! Overflow N
		
		str_p					: in  std_logic_array_Nx1; --! Start P  	
		str_n					: in  std_logic_array_Nx1; --! Start N
				
		clk_p					: in  std_logic_array_Nx1; --! Clock P  	
		clk_n					: in  std_logic_array_Nx1; --! Clock N		
		
		---- ADC Program (SPI interface) Simple example ---- 
		mg_addr					: in  std_logic_vector(07 downto 0); --! Host address
		mg_data_i				: in  std_logic_vector(15 downto 0); --! Host data (in)
		mg_data_o				: out std_logic_vector(15 downto 0); --! Host data (out)
		mg_data_v				: out std_logic; --! Host data valid to out
		mg_start				: in  std_logic; --! Host data start transaction 
		mg_ready				: out std_logic; --! Host ready to send/receive data			
				
		spi_do					: out std_logic; --! SPI: Data out
		spi_di					: in  std_logic; --! SPI: Data in
		spi_ck					: out std_logic; --! SPI: Clock data
		spi_cs					: out std_logic; --! SPI: Chip select (enable)	

		---- System Interface ----
		sys_clk					: in  std_logic; --! System clock
		do_fifo					: out std_logic_vector(PACK_OUT-1 downto 0); --! Fifo data out
		cs_fifo					: in  std_logic; --! Fifo read enable
		ef_fifo					: out std_logic; --! Fifo flag: empty
		ff_fifo					: out std_logic; --! Fifo flag: full
				
		ovr_out					: out std_logic_array_NxK;	--! Flag overflow
		str_out					: out std_logic_array_NxK;	--! Flag start
				
		chan					: in  std_logic_vector(NCHAN-1 downto 0); --! Selected ADC channels
		awen					: in  std_logic; --! Enable for ADC channels		
		---- Start IODELAY ----	
		dl_clk					: in  std_logic:='0'; --! IODELAY clock
		
		dl_chan					: in  std_logic_vector(log2ceil(NCHAN)-1 downto 0); -- Selected mux channel
		dl_muxs					: in  std_logic_vector(log2ceil(DATA_WIDTH)-1 downto 0); -- Selected mux channel	
	
		dl_str_in				: in  std_logic:='0'; --! increment / decrement 		
		dl_str_ce				: in  std_logic:='0'; --! enable for inc/dec
		dl_str_ld				: in  std_logic:='1'; --! load pre-prog delay value
		dl_str_di				: in  std_logic_vector(4 downto 0):=(others=>'0'); 	--! loadable delay value
		dl_str_do				: out std_logic_vector(4 downto 0); --! check delay value		

		---- Data IODELAY ----	
		dl_dat_in				: in  std_logic:='0'; --! increment / decrement 		
		dl_dat_ce				: in  std_logic:='0'; --! enable for inc/dec
		dl_dat_ld				: in  std_logic:='1'; --! load pre-prog delay value
		dl_dat_di				: in  std_logic_vector(4 downto 0):=(others=>'0'); 	--! loadable delay value
		dl_dat_do				: out std_logic_vector(4 downto 0) --! check delay value	
	);
end adc_configurator;

architecture adc_configurator of adc_configurator is

constant DATA_WT		: integer:=DATA_CONFIG*SERDES_WIDTH;
constant DATA_NT		: integer:=DATA_WT*DATA_WIDTH;
constant DATA_PACK		: integer:=NCHAN*DATA_NT;

type std_logicNxM is array (NCHAN-1 downto 0) of std_logic_vector(DATA_NT-1 downto 0);	
type std_logicNx1 is array (NCHAN-1 downto 0) of std_logic_vector(DATA_WT-1 downto 0);	
	
signal dat_out			: std_logicNxM;	
-- signal ovr_out		: std_logicNx1;
-- signal str_out		: std_logicNx1;

---- Repack signals ----
signal di_pack			: std_logic_vector(DATA_PACK-1 downto 0);
signal do_pack			: std_logic_vector(PACK_OUT-1 downto 0);
signal de_pack			: std_logic;
signal dv_pack			: std_logic;

---- MMCM signals declaration ----
signal clk_fb_i			: std_logic;	
signal clk_fb_o			: std_logic;	

signal clk_in1			: std_logic;	
signal clk_in2			: std_logic;
signal pll_lock			: std_logic;
signal pll_rst			: std_logic;

signal clk_0			: std_logic;	
signal clk_1			: std_logic;

---- Find CLOCK DIVIDE for IDDR / SERDES mode ----
function calc_divide(xx, yy : integer) return integer is
begin 
	if (xx = 1) then
		if (yy = 1) then
			return 1;
		else
			return 2;
		end if;
	else
		return xx;
	end if;
end calc_divide;

function calc_serdes(xx : integer) return string is
begin 
	if (xx = 1) then
		return "1";
	elsif (xx = 2) then
		return "2";
	elsif (xx = 4) then
		return "4";
	elsif (xx = 8) then
		return "8";
	end if;
end calc_serdes;
constant CLK_BUFR		: string:=calc_serdes(SERDES_WIDTH);


constant CLK_MULT		: real:=4.0;
constant CLK_DIV		: integer:=1;
constant CLK_DIV0		: real:=CLK_MULT;
constant CLK_DIV1		: integer:=calc_divide(SERDES_WIDTH, DATA_CONFIG)*integer(CLK_MULT);

---- Clocks in the module ----
signal clk_i			: std_logic:='0';
signal aclk				: std_logic:='0';
signal aclk_div			: std_logic:='0';	
signal aclk_glb			: std_logic:='0';	
signal clk_pack			: std_logic:='0';

---- Delays for IOB DELAY ----
signal x_str_in			: std_logic_array_Nx1;		
signal x_str_ce			: std_logic_array_Nx1;
signal x_str_ld			: std_logic_array_Nx1;
signal x_str_di			: std_logic_vector(4 downto 0);
signal x_str_do			: std_logic_array_Nx5;


signal x_dat_in			: std_logic_array_NxM;
signal x_dat_ce			: std_logic_array_NxM;
signal x_dat_ld			: std_logic_array_NxM;
signal x_dat_di			: std_logic_vector(4 downto 0); 
signal x_dat_do			: std_logic_array_Wx5;
signal x_dat_dd			: std_logic_vector(DATA_WIDTH*5-1 downto 0);

signal idelay_rstp		: std_logic; 
signal idelay_reset		: std_logic;

begin  

---- Common SPI interface to manage ADCs ----
xSPI_ADC: ctrl_spi_adc 
	port map (
		clk			=> sys_clk,
		-- sys_clk	=> sys_clk,
		reset		=> reset,

		mg_addr		=> mg_addr,		
		mg_data_i	=> mg_data_i,	
		mg_data_o	=> mg_data_o,	
		mg_data_v	=> mg_data_v,	
		mg_start	=> mg_start,	
		mg_ready	=> mg_ready,
		
		spi_do		=> spi_do,	
		spi_di		=> spi_di,	
		spi_ck		=> spi_ck,	
		spi_cs		=> spi_cs	
	);

---- Clock signal: diff ----
gCLK_DIFF: if (DIFF_CLOCK = TRUE) generate
	xCLK: IBUFDS
		generic map	(
			IOSTANDARD 	=> IOSTANDARD,
			DIFF_TERM 	=> DIFF_CLOCK 
		)
		port map ( 
			o	=> clk_i, 
			i 	=> clk_p(CLK_SEL), 
			ib	=> clk_n(CLK_SEL) 
		);
end generate;

---- Clock signal: signle ----
gCLK_SNGL: if (DIFF_CLOCK = FALSE) generate	
	xCLK: IBUF
		port map ( 
			o	=> clk_i, 
			i 	=> clk_p(CLK_SEL)
		);	
end generate;

---- Select clock to repack ----	
gCLK_SELx1: if (SERDES_WIDTH = 1) generate	
	clk_pack <= aclk_glb;
end generate;
gCLK_SELx8: if (SERDES_WIDTH > 1) generate	
	clk_pack <= aclk_div;
end generate;
	
xREPACK: for ii in 0 to NCHAN-1 generate
	di_pack(DATA_NT*(ii+1)-1 downto DATA_NT*ii) <= dat_out(ii);	
end generate;	
	de_pack <= awen when rising_edge(clk_pack);

---- Repack data from multi-channel ADC ----	
xPACK: ctrl_repackNxM
	generic map (	
		CHAN_NUM	=> NCHAN,
		DATA_WIN	=> DATA_NT,
		DATA_WOUT	=> PACK_OUT
	)
	port map (
		reset		=> reset,
		aclk		=> clk_pack,
	
		chan		=> chan,
		din			=> di_pack,
		den			=> de_pack,
	
		dout		=> do_pack,
		dval		=> dv_pack
	);
	
---- Fifo as RAMB to change data rate (system clk) ----
xFIFO: ctrl_fifo_config 
	generic map (
		DATA_WIDTH	=> PACK_OUT,
		ADDR_WIDTH	=> FIFO_ADDR
	)
	port map (
		reset		=> reset,       
		wr_clk      => clk_pack,	
		rd_clk		=> sys_clk,
		data_i		=> do_pack,
		data_o		=> do_fifo,
		rd_en		=> cs_fifo,
		wr_en		=> dv_pack,	
		empty		=> ef_fifo,
		full		=> ff_fifo
	);

---- Complex data for IODELAY ----
x_str_di <= dl_str_di when rising_edge(dl_clk);	
x_dat_di <= dl_dat_di when rising_edge(dl_clk);	
	
---- Start delays ----
dl_str_do <= x_str_do(conv_integer(unsigned(dl_chan))) when rising_edge(dl_clk);	
	
x_str_in(conv_integer(unsigned(dl_chan))) 	<= dl_str_in when rising_edge(dl_clk);		
x_str_ce(conv_integer(unsigned(dl_chan))) 	<= dl_str_ce when rising_edge(dl_clk);		
x_str_ld(conv_integer(unsigned(dl_chan))) 	<= dl_str_ld when rising_edge(dl_clk);	

---- Data delays ----
x_dat_dd <= x_dat_do(conv_integer(unsigned(dl_chan))) when rising_edge(dl_clk);	
x_dat_in(conv_integer(unsigned(dl_chan)))(conv_integer(unsigned(dl_muxs)))  <= dl_dat_in when rising_edge(dl_clk);		
x_dat_ce(conv_integer(unsigned(dl_chan)))(conv_integer(unsigned(dl_muxs)))  <= dl_dat_ce when rising_edge(dl_clk);	
x_dat_ld(conv_integer(unsigned(dl_chan)))(conv_integer(unsigned(dl_muxs)))  <= dl_dat_ld when rising_edge(dl_clk);	
	
dl_dat_do <= x_dat_dd(4+conv_integer(unsigned(dl_muxs)) downto 0+conv_integer(unsigned(dl_muxs))) when rising_edge(dl_clk);	
	
---- Input buffers for multi-channel ADCs ----		
gCHAN_ADC: for ii in 0 to NCHAN-1 generate
begin

	-- Select delay for starts --
	-- pr_del_st: process(dl_clk)	is
	-- begin
		-- if rising_edge(dl_clk) then
			-- if (dl_chan(ii) = '1') then
				-- x_str_in(ii) 	<= dl_str_in;			
				-- x_str_ce(ii) 	<= dl_str_ce;		
				-- x_str_ld(ii) 	<= dl_str_ld;
			-- else
				-- x_str_in(ii) 	<= '0';			
				-- x_str_ce(ii) 	<= '0';		
				-- x_str_ld(ii) 	<= '0';	
			-- end if;
		-- end if;
	-- end process;
	-- Select channel of delay --	
	-- pr_del_dt: process(dl_clk)	is
	-- begin
		-- if rising_edge(dl_clk) then
			-- if (dl_chan(ii) = '1') then
				-- x_dat_in(ii)(conv_integer(unsigned(dl_muxs)) 	<= dl_dat_in;			
				-- x_dat_ce(ii)(conv_integer(unsigned(dl_muxs)) 	<= dl_dat_ce;		
				-- x_dat_ld(ii)(conv_integer(unsigned(dl_muxs)) 	<= dl_dat_ld;
				-- x_dat_dd <= 
			-- else
				-- x_dat_in(ii)(conv_integer(unsigned(dl_muxs)) 	<= '0';			
				-- x_dat_ce(ii)(conv_integer(unsigned(dl_muxs)) 	<= '0';		
				-- x_dat_ld(ii)(conv_integer(unsigned(dl_muxs)) 	<= '0';	
				-- x_dat_dd
			-- end if;
		-- end if;
	-- end process;		
	
	xADC: adc_receiver    
		generic map (	
			DATA_WIDTH			=> DATA_WIDTH,
			DIFF_TERM			=> DIFF_TERM,
			DATA_CONFIG			=> DATA_CONFIG,			 
			IOSTANDARD			=> IOSTANDARD,			
			DATA_IOBDELAY_TYPE	=> DATA_IOBDELAY_TYPE,	
			DATA_IOBDELAY_VALUE	=> DATA_IOBDELAY_VALUE,	
			SERDES_WIDTH		=> SERDES_WIDTH,
	
			OVR_PRES			=> OVR_PRES,
			STR_PRES			=> STR_PRES,
			RESYNC_OUT			=> RESYNC_OUT
		)
		port map (
			reset				=> reset,		   
			
			adc_p				=> adc_p(ii),
			adc_n				=> adc_n(ii),	
			ovr_p				=> ovr_p(ii),
			ovr_n 				=> ovr_n(ii),
			str_p				=> str_p(ii),
			str_n				=> str_n(ii),
			
			aclk				=> aclk,
			aclk_div			=> aclk_div, 	 
			aclk_glb			=> aclk_glb,
			
			dat_out				=> dat_out(ii),
			ovr_out				=> ovr_out(ii),
			str_out				=> str_out(ii),
			
			dl_str_clk			=> dl_clk,	
			dl_str_in			=> x_str_in(ii),	
			dl_str_ce			=> x_str_ce(ii),	
			dl_str_ld			=> x_str_ld(ii),	
			dl_str_di			=> x_str_di,	
			dl_str_do			=> x_str_do(ii),	
			
			dl_dat_clk			=> dl_clk,
			dl_dat_in			=> x_dat_in(ii),
			dl_dat_ce			=> x_dat_ce(ii),
			dl_dat_ld			=> x_dat_ld(ii),
			dl_dat_di			=> x_dat_di,
			dl_dat_do			=> x_dat_do(ii)
		);
end generate; 


---- Clock synthesis by MMCM/PLL ---- 
xMMCM_CLK: MMCME2_ADV
	generic map (
		BANDWIDTH 			=> "OPTIMIZED", -- Jitter programming (OPTIMIZED, HIGH, LOW)
		CLKFBOUT_MULT_F 	=> CLK_MULT, -- Multiply value for all CLKOUT (2.000-64.000).
		CLKFBOUT_PHASE 		=> 0.0, -- Phase offset in degrees of CLKFB (-360.000-360.000).
		-- CLKIN_PERIOD: Input clock period in ns to ps resolution (i.e. 33.333 is 30 MHz).
		CLKIN1_PERIOD 		=> 5.0,
		CLKIN2_PERIOD 		=> 5.0,
		-- CLKOUT0_DIVIDE - CLKOUT6_DIVIDE: Divide amount for CLKOUT (1-128)
		CLKOUT1_DIVIDE 		=> CLK_DIV1,
		CLKOUT2_DIVIDE 		=> 1,
		CLKOUT3_DIVIDE 		=> 1,
		CLKOUT4_DIVIDE 		=> 1,
		CLKOUT5_DIVIDE 		=> 1,
		CLKOUT6_DIVIDE 		=> 1,
		CLKOUT0_DIVIDE_F 	=> CLK_DIV0,       -- Divide amount for CLKOUT0 (1.000-128.000).
		-- CLKOUT0_PHASE - CLKOUT6_PHASE: Phase offset for CLKOUT outputs (-360.000-360.000).
		CLKOUT0_PHASE 		=> 0.0,
		CLKOUT1_PHASE 		=> 0.0,
		CLKOUT2_PHASE 		=> 0.0,
		CLKOUT3_PHASE 		=> 0.0,
		CLKOUT4_PHASE 		=> 0.0,
		CLKOUT5_PHASE 		=> 0.0,
		CLKOUT6_PHASE 		=> 0.0,

		COMPENSATION 		=> "ZHOLD",  -- ZHOLD, BUF_IN, EXTERNAL, INTERNAL
		DIVCLK_DIVIDE 		=> CLK_DIV,  -- Master division value (1-106)
		-- REF_JITTER: Reference input jitter in UI (0.000-0.999).
		REF_JITTER1 		=> 0.0,
		REF_JITTER2 		=> 0.0
	)
	port map (
		-- Clock Outputs: 1-bit (each) output: User configurable clock outputs
		CLKOUT0 			=> clk_0,   
		CLKOUT0B 			=> open,  
		CLKOUT1 			=> clk_1,   
		CLKOUT1B 			=> open,  
		CLKOUT2 			=> open,   
		CLKOUT2B 			=> open,  
		CLKOUT3 			=> open,  
		CLKOUT3B 			=> open,  
		CLKOUT4 			=> open,  
		CLKOUT5 			=> open,  
		CLKOUT6 			=> open,  

		CLKFBOUT 			=> clk_fb_i,        
		CLKFBOUTB 			=> open,       
		LOCKED 				=> pll_lock,          
		-- Clock Inputs: 1-bit (each) input: Clock inputs
		CLKIN1 				=> clk_in1,           
		CLKIN2 				=> clk_in2,           
		-- Control Ports: 1-bit (each) input: MMCM control ports
		CLKINSEL 			=> '1', 
		PWRDWN 				=> '0',            
		RST 				=> pll_rst,                  
		-- DRP Ports: 7-bit (each) input: Dynamic reconfiguration ports
		DADDR 				=> (others =>'0'), 
		DCLK 				=> '0',            
		DEN 				=> '0',              
		DI 					=> (others =>'0'),   
		DWE 				=> '0',              
		DO 					=> open,     
		DRDY 				=> open,  
		PSCLK 				=> '0',          
		PSEN 				=> '0',           
		PSINCDEC 			=> '0',       
		
		CLKFBIN 			=> clk_fb_o            
	);
	
---- IDELAY CTRL ----
idelay_rstp <= not pll_lock;

xRSTDEL: SRL16 
	port map ( 
		q		=> idelay_reset, 
		clk		=> sys_clk, 
		d		=> idelay_rstp, 
		a3		=> '1', 
		a2		=> '1', 
		a1		=> '1', 
		a0		=> '1'
	);	
	
xDELCTRL: IDELAYCTRL
	port map (
		-- RDY 	=> RDY,    
		REFCLK 	=> sys_clk, 
		RST 	=> idelay_reset     
	);	
	
pll_rst <= (not reset) when rising_edge(sys_clk);

xCLK_FB: bufg port map ( i => clk_fb_i, o => clk_fb_o );
	
clk_in1 <= aclk;	
clk_in2 <= aclk;

aclk_glb <= clk_0;	

xPLL_TRUE: if (USE_MMCM = TRUE) generate	
	aclk_div <= clk_1;	
end generate;

---- Clock global buffer ----
XCLKB: BUFG 
	port map (
		o	=> aclk, 
		i	=> clk_i 
	);

xPLL_FALSE: if (USE_MMCM = FALSE) generate
	xBUFR : BUFR
		generic map (
		BUFR_DIVIDE => CLK_BUFR  -- 1, 2, 3, 4, 5, 6, 7, 8
	)
	port map (
		O 		=> aclk_div,  
		CE 		=> '1', 
		CLR 	=> '0',
		I 		=> clk_i   
	);
end generate;	
	
end adc_configurator;