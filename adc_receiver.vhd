-------------------------------------------------------------------------------
--
-- Title		: adc_receiver.vhd
-- Author		: Alexander Kapitanov
-- Company		: ...
-- E-mail		: sallador@bk.ru
--
-- Version		: 1.0	
--
-------------------------------------------------------------------------------
--
-- Description :  Прием данных с субмодуля
--			- До 16 бит, LVDS/LVTTL/LVPECL, 
--			- DDR/SDR, снижение битрейта в 2/4/8 раз (см. SERDES)
--			- При некратной разрядности выравнивание данных по старшему биту
--			- Отдельные каналы для сигналов старта и переполнения
--			- В режиме DDR используется SERDES_WIDTH = x1	 
--			- В режиме SDR используется SERDES_WIDTH > 0	 
--			- Использование дифференциальных линий данных и такта	
--			- IODELAY в OVR программируется вместе с 0 битом данных - ADC DATA[0]			 
--
--
-------------------------------------------------------------------------------
--
--  Version 1.0   07.07.2017
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package adc_receiver_pkg is	
	component adc_receiver is	   
		generic (	
			DATA_WIDTH				: integer 	:=16; --! Разрядность данных на входе буфера
			
			DIFF_TERM				: boolean	:=TRUE; --! Использование дифференциальных сигналов (TRUE/FALSE)
			DATA_CONFIG				: integer 	:=1; --! Режим приёма данных: 1 - SDR / 2- DDR (для SERDES_WIDTH = x1)
			IOSTANDARD				: string 	:="LVDS_25"; --! Стандарт линий для LVDS
			DATA_IOBDELAY_TYPE		: string 	:="VARIABLE"; --! Режим работы узла задержки IODELAY
			DATA_IOBDELAY_VALUE		: integer 	:=0; --! Значение задержки в режиме "FIXED"

			SERDES_WIDTH			: natural	:=4; --! Разрядность SERDES линии х1, х2, х4, х8 (х1 - если SERDES не используется)

			OVR_PRES				: string 	:="NO"; --! Использование линий переполнения 
			STR_PRES				: string 	:="NO"; --! Использование линий старта
			RESYNC_OUT				: string 	:="NO" --! Использование механизма пересинхронизации выходов через FIFO (NO -  регистр)
			--INV_MASK				: std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'0')  -- маска инверсии по входу
		);
		port (
			-- Common Interface ----
			reset					: in  std_logic;		--! 0 - Сброс		   
			---- ADC Interface ---- 
			adc_p					: in  std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'0'); --! Data input P
			adc_n					: in  std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'1'); --! Data input N 
					
			ovr_p					: in  std_logic:='0';	 	--! Overflow P 
			ovr_n 					: in  std_logic:='1';	 	--! Overflow N
		
			str_p					: in  std_logic:='0';	  	--! Start P  	
			str_n					: in  std_logic:='1';	  	--! Start N
			
			-- clk_p				: in  std_logic:='0';	  	--! Clock P  	
			-- clk_n				: in  std_logic:='1';	  	--! Clock N		
			
			aclk					: in  std_logic;  --! Input data clock (Global buffer)
			---- Data Interface ----
			dat_out					: out std_logic_vector(DATA_CONFIG*DATA_WIDTH*SERDES_WIDTH-1 downto 0);	--! Data output
			ovr_out					: out std_logic_vector(DATA_CONFIG*SERDES_WIDTH-1 downto 0);	--! Flag overflow
			str_out					: out std_logic_vector(DATA_CONFIG*SERDES_WIDTH-1 downto 0);	--! Flag start
			
			aclk_div				: in  std_logic:='0';  	--! Тактовая частота для выходных данных (aclk/2 для DDR, aclk/4 для SDR)  
			aclk_glb				: in  std_logic:='0';	--! Тактовая частота для выходных данных с глобального буфера
	
			---- Start IODELAY ----	
			dl_str_clk				: in  std_logic:='0'; --! clock
			dl_str_in				: in  std_logic:='0'; --! increment / decrement 		
			dl_str_ce				: in  std_logic:='0'; --! enable for inc/dec
			dl_str_ld				: in  std_logic:='1'; --! load pre-prog delay value
			dl_str_di				: in  std_logic_vector(4 downto 0):=(others=>'0'); 	--! loadable delay value
			dl_str_do				: out std_logic_vector(4 downto 0); --! check delay value		
			
			---- Data IODELAY ----	
			dl_dat_clk				: in  std_logic:='0'; --! clock
			dl_dat_in				: in  std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'0'); --! increment / decrement 		
			dl_dat_ce				: in  std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'0'); --! enable for inc/dec
			dl_dat_ld				: in  std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'1'); --! load pre-prog delay value
			dl_dat_di				: in  std_logic_vector(4 downto 0):=(others=>'0'); 	--! loadable delay value
			dl_dat_do				: out std_logic_vector(DATA_WIDTH*5-1 downto 0) --! check delay value	
			
			---- Control SERDES out ---
			--ser_dat					: out std_logic_vector(DATA_WIDTH-1 downto 0)
		);
	end component;
end package;

library ieee;
use ieee.std_logic_1164.all;	   
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

library unisim;
use unisim.vcomponents.all;

-- synopsys translate_off
library ieee;
use ieee.vital_timing.all;
-- synopsys translate_on  

-- library work;
-- use work.adc_package.all;

entity adc_receiver is	   
	generic (	
		DATA_WIDTH				: integer 	:=16; --! Разрядность данных на входе буфера
		
		DIFF_TERM				: boolean	:=TRUE; --! Использование дифференциальных сигналов (TRUE/FALSE)
		DATA_CONFIG				: integer 	:=1; --! Режим приёма данных: 1 - SDR / 2- DDR (для SERDES_WIDTH = x1)
		IOSTANDARD				: string 	:="LVDS_25"; --! Стандарт линий для LVDS
		DATA_IOBDELAY_TYPE		: string 	:="VARIABLE"; --! Режим работы узла задержки IODELAY
		DATA_IOBDELAY_VALUE		: integer 	:=0; --! Значение задержки в режиме "FIXED"

		SERDES_WIDTH			: natural	:=4; --! Разрядность SERDES линии х1, х2, х4, х8 (х1 - если SERDES не используется)

		OVR_PRES				: string 	:="NO"; --! Использование линий переполнения 
		STR_PRES				: string 	:="NO"; --! Использование линий старта
		RESYNC_OUT				: string 	:="NO" --! Использование механизма пересинхронизации выходов через FIFO (NO -  регистр)
		--INV_MASK				: std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'0')  -- маска инверсии по входу
	);
	port (
		-- Common Interface ----
		reset			: in  std_logic;		--! 0 - Сброс		   
		---- ADC Interface ---- 
		adc_p			: in  std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'0'); --! Data input P
		adc_n			: in  std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'1'); --! Data input N 
		
		ovr_p			: in  std_logic:='0';	 	--! Overflow P 
		ovr_n 			: in  std_logic:='1';	 	--! Overflow N

		str_p			: in  std_logic:='0';	  	--! Start P  	
		str_n			: in  std_logic:='1';	  	--! Start N
		
		-- clk_p			: in  std_logic:='0';	  	--! Clock P  	
		-- clk_n			: in  std_logic:='1';	  	--! Clock N		
		
		aclk			: in  std_logic;  --! Input data clock (Global buffer)
		---- Data Interface ----
		dat_out			: out std_logic_vector(DATA_CONFIG*DATA_WIDTH*SERDES_WIDTH-1 downto 0);	--! Data output
		ovr_out			: out std_logic_vector(DATA_CONFIG*SERDES_WIDTH-1 downto 0);	--! Flag overflow
		str_out			: out std_logic_vector(DATA_CONFIG*SERDES_WIDTH-1 downto 0);	--! Flag start
		
		aclk_div		: in  std_logic:='0';  	--! Тактовая частота для выходных данных (aclk/2 для DDR, aclk/4 для SDR)  
		aclk_glb		: in  std_logic:='0';	--! Тактовая частота для выходных данных с глобального буфера
		
		---- Start IODELAY ----	
		dl_str_clk		: in  std_logic:='0'; --! clock
		dl_str_in		: in  std_logic:='0'; --! increment / decrement 		
		dl_str_ce		: in  std_logic:='0'; --! enable for inc/dec
		dl_str_ld		: in  std_logic:='1'; --! load pre-prog delay value
		dl_str_di		: in  std_logic_vector(4 downto 0):=(others=>'0'); 	--! loadable delay value
		dl_str_do		: out std_logic_vector(4 downto 0); --! check delay value		
		
		---- Data IODELAY ----		
		dl_dat_clk		: in  std_logic:='0'; --! clock
		dl_dat_in		: in  std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'0'); --! increment / decrement 		
		dl_dat_ce		: in  std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'0'); --! enable for inc/dec
		dl_dat_ld		: in  std_logic_vector(DATA_WIDTH-1 downto 0):=(others=>'1'); --! load pre-prog delay value
		dl_dat_di		: in  std_logic_vector(4 downto 0):=(others=>'0'); 	--! loadable delay value
		dl_dat_do		: out std_logic_vector(DATA_WIDTH*5-1 downto 0) --! check delay value	
		
		---- Control SERDES out ---
		--ser_dat			: out std_logic_vector(DATA_WIDTH-1 downto 0)
	);
end adc_receiver;

architecture adc_receiver of adc_receiver is

---- SIGNAL DECLARATION ---- 
signal adc_x		: std_logic_vector(DATA_WIDTH-1 downto 0);
signal adc_d		: std_logic_vector(DATA_WIDTH-1 downto 0);
signal rst			: std_logic_vector(DATA_WIDTH-1 downto 0);

function calc_mode(xx : integer) return string is
begin 
	if (DATA_CONFIG = 1) then
		return "SDR";
	else
		return "DDR";
	end if;
end calc_mode;
constant DATA_RATE 	: string:=calc_mode(DATA_CONFIG);

function calc_width(xx, yy : integer) return integer is
begin 
	if (xx = 1) then
		if (yy = 1) then
			return 1;
		else
			return 2;
		end if;
	else
		return 8;
	end if;
end calc_width;
constant NWIDTH 	: integer:=calc_width(SERDES_WIDTH, DATA_CONFIG);

constant DT			: integer:= DATA_CONFIG*SERDES_WIDTH;	
constant DW			: integer:= DT*DATA_WIDTH;	

signal dat_x		: std_logic_vector(NWIDTH*DATA_WIDTH-1 downto 0);
signal rstp			: std_logic;
signal aclk_n		: std_logic;
--signal sdat_clk		: std_logic;

signal str_x		: std_logic_vector(NWIDTH-1 downto 0);
signal ovr_x		: std_logic_vector(NWIDTH-1 downto 0);

component ctrl_fifo_config is
    generic (
		DATA_WIDTH		: integer:= 8;
		ADDR_WIDTH		: integer:= 4 
    );
    port (	       
		reset			: in  std_logic;		       
		wr_clk          : in  std_logic;
        rd_clk			: in  std_logic;
        data_i			: in  std_logic_vector(DATA_WIDTH-1 downto 0);
        data_o			: out std_logic_vector(DATA_WIDTH-1 downto 0);
        rd_en			: in  std_logic;
        wr_en			: in  std_logic;
        empty			: out std_logic;
        full			: out std_logic 
    );
end component;

-- function calc_mux(xx: integer) return integer is
-- begin 
	-- if (xx < 2) then
		-- return 1;
	-- elsif ((xx > 2) and (xx < 5)) then
		-- return 2;
	-- elsif ((xx > 4) and (xx < 9)) then
		-- return 3;		
	-- elsif ((xx > 8) and (xx < 17)) then
		-- return 4;		
	-- else
		-- return 5;
	-- end if;
-- end calc_mux;
-- constant NMUX 	: integer:=calc_mux(DATA_WIDTH);
-- type std_logic_array_Nx5 is array (NMUX-1 downto 0) of std_logic_vector(4 downto 0);	
-- signal dat_delay	: std_logic_array_Nx5;

begin  
	
--dl_dat_do <= dat_delay(CONV_INTEGER(UNSIGNED(dl_dat_mux))) when rising_edge(dl_dat_clk);
	
rstp <= not reset;
--sdat_clk <= aclk_div;

---- start signal: false ----
gST_FALSE: if (STR_PRES = "NO") generate 
	str_x	<= x"00";
end generate;

---- start signal: true ----
gST_TRUE_DIFF: if (STR_PRES = "YES") generate 
	signal str_o		: std_logic;
	signal str_i		: std_logic;	
begin	
	---- diff term usage: true/false ----
	gDIFF: if (DIFF_TERM = TRUE) generate
		xSTART: IBUFDS
			generic map	(
				IOSTANDARD 	=> IOSTANDARD,
				DIFF_TERM 	=> DIFF_TERM 
			)
			port map ( 
				o	=> str_i, 
				i 	=> str_p, 
				ib	=> str_n 
			);
	end generate;
	
	gSNGL: if (DIFF_TERM = FALSE) generate	
		xSTART: IBUF
			port map ( 
				o	=> str_i, 
				i 	=> str_p
			);	
	end generate;
	
	---- iodelay block ----
	xIDELAY: IDELAYE2 
		generic map (
			IDELAY_TYPE		=> DATA_IOBDELAY_TYPE, -- "DEFAULT", "FIXED", "VARIABLE"
			IDELAY_VALUE	=> DATA_IOBDELAY_VALUE -- Delay value: 0 to 31
		)
		port map (
			DATAIN			=> '0',		-- data from FPGA logic		
			IDATAIN 		=> str_i,		-- data from IO PAD
			DATAOUT			=> str_o,		-- data out
		  
			C				=> dl_str_clk,	-- clock
			CE				=> dl_str_ce, 	-- clock enable for inc/dec
			INC				=> dl_str_in,	-- increment / decrement 
			LD 				=> dl_str_ld,	-- load pre-prog delay value
			CNTVALUEIN		=> dl_str_di,	-- loadable delay value
			CNTVALUEOUT		=> dl_str_do,	-- check delay value
				               
			REGRST			=> '0',
			CINVCTRL		=> '0',
			LDPIPEEN		=> '0' 
		);	
	
	---- single start ----
	gST_x1: if (SERDES_WIDTH = 1) generate
		gSDR: if (DATA_RATE = "SDR") generate
			str_x(0) <= str_o when rising_edge(aclk);
		end generate;
		gDDR: if (DATA_RATE = "DDR") generate
			xDDR : IDDR 
				generic map ( DDR_CLK_EDGE 	=> "SAME_EDGE_PIPELINED" -- "OPPOSITE/SAME EDGE"
				) 
				port map (
					Q1	=> str_x(1), 
					Q2	=> str_x(0), 
					C	=> aclk,  
					CE	=> '1', 
					D	=> str_o,  
					R	=> '0',  
					S	=> '0'   
				);			
		end generate;		
	end generate;
	
	---- multiply start ----
	gST_x8: if (SERDES_WIDTH > 1) generate 
		signal rst_str		: std_logic;
		signal str_tst		: std_logic;
	begin
		xISERDES: ISERDESE2
			generic map (
				DATA_RATE 	=> DATA_RATE, -- SDR / DDR
				DATA_WIDTH 	=> SERDES_WIDTH, 
				-- for ddr: 4,6,8, or 10
				-- for sdr: 2,3,4,5,6,7, or 8
				INTERFACE_TYPE	=> "NETWORKING",	-- use model - "memory" or "networking"
				NUM_CE			=> 1, 				-- number or clock enables: 1 or 2
				IOBDELAY		=> "BOTH",
				SERDES_MODE 	=> "MASTER" --set serdes mode to "master" or "slave"
				)
			port map (
				-- Registered outputs
				Q1 				=> str_x(0), 
				Q2 				=> str_x(1), 
				Q3 				=> str_x(2), 
				Q4 				=> str_x(3),
				Q5 				=> str_x(4), 
				Q6 				=> str_x(5), 
				Q7 				=> str_x(6), 
				Q8 				=> str_x(7), 		
				-- Unregistered output   
				O				=> str_tst, -- control data from start
				-- Carry out for bit expansion
				SHIFTOUT1 		=> open, 
				SHIFTOUT2 		=> open, 
				-- Serial data in from PAD or IODELAY
				D				=> '0', --str_i	
				DDLY 			=> str_o,
				-- Carry in for bit expansion
				SHIFTIN1 		=> '0', 
				SHIFTIN2 		=> '0', 
				-- Clock signals 
				CLK 			=> aclk,	-- high-speed clock 
				CLKB 			=> aclk_n,	-- inverted clock
				CLKDIV 			=> aclk_div,-- divided clock		
				-- Clock enable
				CE1				=> '1',
				CE2				=> '1',
				-- Reset
				RST				=> rst_str,  
				--- NOT USED	
				BITSLIP			=> '0', -- bitslip operation
				OCLK			=> '0', -- high-speed clock
				OCLKB			=> '0', -- inverted clock
		
				DYNCLKSEL		=> '0',
				DYNCLKDIVSEL	=> '0',
				CLKDIVP			=> '0',
				OFB				=> '0' -- feedback path
			);	
	
		xRST: FD 
			port map (
				Q => rst_str,
				D => rstp,
				C => aclk_div
			);  
	end generate;	
end generate;


---- overflow signal: false ----
gOVR_FALSE: if (OVR_PRES = "NO") generate 
	ovr_x <= x"00";
end generate;

---- overflow signal: true ----
gOVR_TRUE_DIFF: if (OVR_PRES = "YES") generate 
	signal ovr_o		: std_logic;
	signal ovr_i		: std_logic;	
begin	
	---- diff term usage: true/false ----
	gDIFF: if (DIFF_TERM = TRUE) generate
		xOVR: IBUFDS
			generic map	(
				IOSTANDARD 	=> IOSTANDARD,
				DIFF_TERM 	=> DIFF_TERM 
			)
			port map ( 
				o	=> ovr_i, 
				i 	=> ovr_p, 
				ib	=> ovr_n 
			);
	end generate;
	
	gSNGL: if (DIFF_TERM = FALSE) generate	
		xOVR: IBUF
			port map ( 
				o	=> ovr_i, 
				i 	=> ovr_p
			);	
	end generate;
	
	---- iodelay block ----
	xIDELAY: IDELAYE2 
		generic map (
			IDELAY_TYPE		=> DATA_IOBDELAY_TYPE, -- "DEFAULT", "FIXED", "VARIABLE"
			IDELAY_VALUE	=> DATA_IOBDELAY_VALUE -- Delay value: 0 to 31
		)
		port map (
			DATAIN			=> '0',	-- data from FPGA logic		
			IDATAIN 		=> ovr_i,	-- data from IO PAD     
			DATAOUT			=> ovr_o,	-- data out             
		  
			C				=> dl_dat_clk,	-- clock
			CE				=> dl_dat_ce(DATA_WIDTH*0), 	-- clock enable for inc/dec
			INC				=> dl_dat_in(DATA_WIDTH*0),	-- increment / decrement 
			LD 				=> dl_dat_ld(DATA_WIDTH*0),	-- load pre-prog delay value
			CNTVALUEIN		=> dl_dat_di,	-- loadable delay value
			--CNTVALUEOUT	=> dl_dat_do,	-- check delay value
				               
			REGRST			=> '0',
			CINVCTRL		=> '0',
			LDPIPEEN		=> '0' 
		);

	---- single overflow ----
	gOVR_x1: if (SERDES_WIDTH = 1) generate 
		gSDR: if (DATA_RATE = "SDR") generate
			ovr_x(0) <= ovr_o when rising_edge(aclk);
		end generate;
		gDDR: if (DATA_RATE = "DDR") generate
			xDDR : IDDR 
				generic map ( DDR_CLK_EDGE 	=> "SAME_EDGE_PIPELINED" -- "OPPOSITE/SAME EDGE"
				) 
				port map (
					Q1	=> ovr_x(1), 
					Q2	=> ovr_x(0), 
					C	=> aclk,  
					CE	=> '1', 
					D	=> ovr_o,  
					R	=> '0',  
					S	=> '0'   
				);			
		end generate;			
	end generate;
	
	---- multiply overflow ----
	gOVR_x8: if (SERDES_WIDTH > 1) generate 
		signal rst_ovr		: std_logic;
	begin		
		xISERDES: ISERDESE2
			generic map (
				DATA_RATE 	=> DATA_RATE, -- SDR / DDR
				DATA_WIDTH 	=> SERDES_WIDTH, 
				-- for ddr: 4,6,8, or 10
				-- for sdr: 2,3,4,5,6,7, or 8
				INTERFACE_TYPE	=> "NETWORKING",	-- use model - "memory" or "networking"
				NUM_CE			=> 1, 				-- number or clock enables: 1 or 2
				IOBDELAY		=> "BOTH",
				SERDES_MODE 	=> "MASTER" --set serdes mode to "master" or "slave"
				)
			port map (
				-- Registered outputs
				Q1 				=> ovr_x(0), 
				Q2 				=> ovr_x(1), 
				Q3 				=> ovr_x(2), 
				Q4 				=> ovr_x(3),
				Q5 				=> ovr_x(4), 
				Q6 				=> ovr_x(5), 
				Q7 				=> ovr_x(6), 
				Q8 				=> ovr_x(7), 	
				-- Unregistered output
				O				=> open,
				-- Carry out for bit expansion
				SHIFTOUT1 		=> open, 
				SHIFTOUT2 		=> open, 
				-- Serial data in from PAD or IODELAY
				D				=> '0', 	
				DDLY 			=> ovr_o,
				-- Carry in for bit expansion
				SHIFTIN1 		=> '0', 
				SHIFTIN2 		=> '0', 
				-- Clock signals 
				CLK 			=> aclk,	-- high-speed clock 
				CLKB 			=> aclk_n,	-- inverted clock
				CLKDIV 			=> aclk_div,-- divided clock		
				-- Clock enable
				CE1				=> '1',
				CE2				=> '1',
				-- Reset
				RST				=> rst_ovr,  
				--- NOT USED	
				BITSLIP			=> '0', -- bitslip operation
				OCLK			=> '0', -- high-speed clock
				OCLKB			=> '0', -- inverted clock
			
				DYNCLKSEL		=> '0',
				DYNCLKDIVSEL	=> '0',
				CLKDIVP			=> '0',
				OFB				=> '0' -- feedback path
			);	
   
		xRST: FD 
			port map (
				Q => rst_ovr,
				D => rstp,
				C => aclk_div
			);  
	end generate;			
end generate;			

---- adc data ----
gADC_IN: for ii in 0 to (DATA_WIDTH-1) generate
	---- diff term usage: true/false ----
	gDIFF: if (DIFF_TERM = TRUE) generate
		xADC : IBUFDS 
			generic map (
				IOSTANDARD 	=> IOSTANDARD,
				DIFF_TERM 	=> DIFF_TERM)
			port map (
				o	=> 	adc_x(ii),
				i 	=> 	adc_p(ii),
				ib	=> 	adc_n(ii)
			);
	end generate;
	
	gSNGL: if (DIFF_TERM = FALSE) generate
		xADC : IBUF 
			port map (
				o	=> 	adc_x(ii),
				i 	=> 	adc_p(ii)
			);		
	end generate;
	---- iodelay data block ----
	xIDELAY: IDELAYE2 
		generic map (
			IDELAY_TYPE		=> DATA_IOBDELAY_TYPE, -- "DEFAULT", "FIXED", "VARIABLE"
			IDELAY_VALUE	=> DATA_IOBDELAY_VALUE -- Delay value: 0 to 31
		)
		port map (
			DATAIN			=> '0',	-- data from FPGA logic		
			IDATAIN 		=> adc_x(ii),	-- data from IO PAD
			DATAOUT			=> adc_d(ii),	-- data out
	
			C				=> dl_dat_clk,	-- clock
			CE				=> dl_dat_ce(ii), 	-- clock enable for inc/dec
			INC				=> dl_dat_in(ii),	-- increment / decrement 
			LD 				=> dl_dat_ld(ii),	-- load pre-prog delay value
			CNTVALUEIN		=> dl_dat_di,	-- loadable delay value
			CNTVALUEOUT		=> dl_dat_do(5*(ii+1)-1 downto 5*ii),	-- check delay value
	
			REGRST			=> '0',
			CINVCTRL		=> '0',
			LDPIPEEN		=> '0' 
		);
		
	---- Serial -> Serial ----	
	gDT_x1: if (SERDES_WIDTH = 1) generate 
		gSDR: if (DATA_RATE = "SDR") generate
			dat_x(ii) <= adc_d(ii) when rising_edge(aclk);
		end generate;
		gDDR: if (DATA_RATE = "DDR") generate
			xDDR : IDDR 
				generic map ( DDR_CLK_EDGE 	=> "SAME_EDGE_PIPELINED" -- "OPPOSITE/SAME EDGE"
				) 
				port map (
					Q1	=> dat_x(2*ii+1), -- Posedge data output
					Q2	=> dat_x(2*ii), -- Negedge data output 
					C	=> aclk,  
					CE	=> '1', 
					D	=> adc_d(ii),  
					R	=> '0',  
					S	=> '0'   
				);			
		end generate;		
	end generate;
	
	---- Serial -> Parallel ----
	gDT_x8: if (SERDES_WIDTH > 1) generate 		
		xISERDES: ISERDESE2
			generic map (
				DATA_RATE 	=> DATA_RATE, -- SDR / DDR
				DATA_WIDTH 	=> SERDES_WIDTH, 
				-- for ddr: 4,6,8, or 10
				-- for sdr: 2,3,4,5,6,7, or 8
				INTERFACE_TYPE	=> "NETWORKING",	-- use model - "memory" or "networking"
				NUM_CE			=> 1, 				-- number or clock enables: 1 or 2
				IOBDELAY		=> "BOTH",
				SERDES_MODE 	=> "MASTER" --set serdes mode to "master" or "slave"
				)
			port map (
				-- Registered outputs
				Q1 				=> dat_x((ii)+0*DATA_WIDTH), 
				Q2 				=> dat_x((ii)+1*DATA_WIDTH), 
				Q3 				=> dat_x((ii)+2*DATA_WIDTH), 
				Q4 				=> dat_x((ii)+3*DATA_WIDTH), 
				Q5 				=> dat_x((ii)+4*DATA_WIDTH), 
				Q6 				=> dat_x((ii)+5*DATA_WIDTH), 
				Q7 				=> dat_x((ii)+6*DATA_WIDTH), 
				Q8 				=> dat_x((ii)+7*DATA_WIDTH), 		
				-- Unregistered output
				O				=> open, --ser_dat(ii),
				-- Carry out for bit expansion
				SHIFTOUT1 		=> open, 
				SHIFTOUT2 		=> open, 
				-- Serial data in from PAD or IODELAY
				D				=> '0', 	
				DDLY 			=> adc_d(ii),
				-- Carry in for bit expansion
				SHIFTIN1 		=> '0', 
				SHIFTIN2 		=> '0', 
				-- Clock signals 
				CLK 			=> aclk,	-- high-speed clock 
				CLKB 			=> aclk_n,	-- inverted clock
				CLKDIV 			=> aclk_div,-- divided clock		
				-- Clock enable
				CE1				=> '1',
				CE2				=> '1',
				-- Reset
				RST				=> rst(ii),  
				--- NOT USED	
				BITSLIP			=> '0', -- bitslip operation
				OCLK			=> '0', -- high-speed clock
				OCLKB			=> '0', -- inverted clock
		
				DYNCLKSEL		=> '0',
				DYNCLKDIVSEL	=> '0',
				CLKDIVP			=> '0',
				OFB				=> '0' -- feedback path
	
			);	
			
		xRST : FD port map (
			q => rst(ii),
			d => rstp,
			c => aclk_div
		);			
	end generate;
end generate;	

---- inverse adc data ----
-- adc_d <= adc_x xor INV_MASK;
aclk_n <= not aclk; 

---- No resync output ---- 
gSYNC_NO: if (RESYNC_OUT = "NO") generate
	xDAT: for ii in 0 to DT-1 generate
		str_out(ii) <= str_x(DT-1-ii) when rising_edge(aclk_glb);
		ovr_out(ii) <= ovr_x(DT-1-ii) when rising_edge(aclk_glb);
		dat_out(DATA_WIDTH*(ii+1)-1 downto DATA_WIDTH*(ii)) <= dat_x(DATA_WIDTH*(DT-ii)-1 downto DATA_WIDTH*(DT-1-ii)) when rising_edge(aclk_glb);
	end generate;
end generate;

---- Resync output ---- 
gSYNC_YES: if (RESYNC_OUT = "YES") generate 
	signal fifo_i0		: std_logic_vector(DT-1 downto 0);
	signal fifo_i1		: std_logic_vector(DT-1 downto 0);	
	signal fifo_i2		: std_logic_vector(DW-1 downto 0);	
	signal fifo_i		: std_logic_vector(DW+2*DT-1 downto 0);
	signal fifo_o		: std_logic_vector(DW+2*DT-1 downto 0);
	signal fifo_r		: std_logic; -- read fifo
	signal full			: std_logic; -- full fifo	
	signal empty		: std_logic; -- full fifo	
	signal start_read	: std_logic:='0'; -- start read
begin
	---- Data repack first stage ----	
	xDAT: for ii in 0 to DT-1 generate	
		fifo_i0(ii) <= str_x(DT-1-ii);
		fifo_i1(ii) <= ovr_x(DT-1-ii);		
		fifo_i2(DATA_WIDTH*(ii+1)-1 downto 1*DATA_WIDTH*(ii)) <= dat_x(DATA_WIDTH*(DT-ii)-1 downto DATA_WIDTH*(DT-1-ii));
	end generate;	
	
	fifo_i <= fifo_i0 & fifo_i1 & fifo_i2;
	---- FIFO output ----
	xSTD_FIFO : ctrl_fifo_config	
		generic map (
			DATA_WIDTH		=> DW+2*DT,
			ADDR_WIDTH		=> 4
		)
		port map (	       
			reset			=> rstp, 	       
			wr_clk          => aclk,
			rd_clk			=> aclk_glb,
			data_i			=> fifo_i,--fifo_i,
			data_o			=> fifo_o,
			rd_en			=> fifo_r,
			wr_en			=> '1',		
			empty			=> empty,
			full			=> full
		);	
	start_read <= '0' when (rstp = '1') else '1' when (empty = '0');
	fifo_r <= start_read when rising_edge(aclk_glb);
	
	dat_out	<= fifo_o(DW+0*DT-1 downto 0*DW+0*DT);
	ovr_out	<= fifo_o(DW+1*DT-1 downto 1*DW+0*DT);
	str_out	<= fifo_o(DW+2*DT-1 downto 1*DW+1*DT);
end generate;

end adc_receiver;