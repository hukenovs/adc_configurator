-------------------------------------------------------------------------------
--
-- Title		: ctrl_repackNxM.vhd
-- Author		: Alexander Kapitanov
-- Company		: insys.ru
-- E-mail		: sallador@bk.ru
--
-- Version		: 1.0	
--
-------------------------------------------------------------------------------
--
-- Description :  Узел перепаковки данных для FIFO АЦП
--				  -	8 ind. channels
--				  - Wide multiplexer to channels.
--				  - When CHAN = 1, DATA_WIN = 128, DATA_WOUT = 64 you cannot configure repack.
--				  - When CHAN > 1, you cannot configure repack with some data in/out WIDTH.
--				  - Data in WIDTH: 8, 12, 14, 16, 32, 64, 128.
--				  - Data out WIDTH: 64, 128, 256, 512. (you can set 32 for tests)
--
--				 
-------------------------------------------------------------------------------
--
--  Version 1.0   07.07.2016
--
-------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;

package ctrl_repackNxM_pkg is	
	component ctrl_repackNxM is
		generic (	
			CHAN_NUM	: integer:=4;
			DATA_WIN	: integer:=16;
			DATA_WOUT	: integer:=256		
		);
		port (
			reset		: in  std_logic:='0'; --! 0 - global reset
			aclk		: in  std_logic:='0'; --! Clock

			chan		: in  std_logic_vector(CHAN_NUM-1 downto 0):=(others=>'0'); --! Selected chan
			din			: in  std_logic_vector(DATA_WIN*CHAN_NUM-1 downto 0):=(others=>'0'); --! DataIn from ADC
			den			: in  std_logic:='0'; --! Data enable
		
			dout		: out std_logic_vector(DATA_WOUT-1 downto 0 ); --! Data out (packed)
			dval		: out std_logic	--! Valid (enable for FIFO ADC)
		);
	end component;
end package;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

entity ctrl_repackNxM is
	generic (	
		CHAN_NUM	: integer:=4;
		DATA_WIN	: integer:=16;
		DATA_WOUT	: integer:=256		
	);
	port (
		reset		: in  std_logic:='0'; --! 0 - global reset
		aclk		: in  std_logic:='0'; --! Clock
		                  
		chan		: in  std_logic_vector(CHAN_NUM-1 downto 0):=(others=>'0'); --! Selected chan
		din			: in  std_logic_vector(DATA_WIN*CHAN_NUM-1 downto 0):=(others=>'0'); --! DataIn from ADC
		den			: in  std_logic:='0'; --! Data enable
	
		dout		: out std_logic_vector(DATA_WOUT-1 downto 0 ); --! Data out (packed)
		dval		: out std_logic	--! Valid (enable for FIFO ADC)
	);
end entity;

architecture ctrl_repackNxM of ctrl_repackNxM is

---------------------------------
-- wi/wo 64    128    256    512

-- 008    8     16     32     64
-- 012    4      8     16     32
-- 014    4      8     16     32
-- 016    4      8     16     32
-- 032    2      4      8     16
-- 064    1      2      4      8
-- 128    0      1      2      4
---------------------------------
function pack_dat (wi, wo: in integer) return integer is
variable nwi	: integer;
variable nwo	: integer;
begin
	case wi is 
		when 000008 => nwi := 64;--8;
		when 000012 => nwi := 32;--4;
		when 000014 => nwi := 32;--4;
		when 000016 => nwi := 32;--4;
		when 000032 => nwi := 16;--2;
		when 000064 => nwi := 08;--1;
		when 000128 => nwi := 04;--0;
		when others => nwi := 00;--0;
	end case;
	nwo := (nwi * wo)/512;
	return nwo;
end;
-- 1 chan:
---------------------------------
-- wi/wo 64    128    256    512

-- 008    8     16     32     64
-- 016    4      8     16     32
-- 032    2      4      8     16
-- 064    1      2      4      8
-- 128    0      1      2      4
---------------------------------
constant DXMUX : integer:=pack_dat(DATA_WIN, DATA_WOUT);
constant DCHAN : integer:=DXMUX/CHAN_NUM;
-- Cannot pack chan = 1 / width = 128 into 64 bit fifo.
-- Cannot pack some channels into N width fifo.

signal dena		: std_logic;
signal dpack	: std_logic_vector(DATA_WOUT-1 downto 0):=(others=>'0');

begin

dval <= dena when rising_edge(aclk);
 
pr_out: process(aclk, reset) is
begin
	if rising_edge(aclk) then
		if (reset = '0') then
			dout <= (others => '0');
		elsif (dena = '1') then
			dout <= dpack;
		end if;
	end if;
end process;

xFAIL: if (DCHAN = 0) generate
	assert (DCHAN /= 0) report "CANNOT PACK DATA INTO FIFO" severity error;
end generate;

xPASS: if (DCHAN /= 0) generate
	signal ddat		: std_logic;
	signal dmux		: std_logic_vector(DCHAN-1 downto 0);
	signal dsel		: std_logic_vector(DCHAN-1 downto 0);
	signal ddis		: std_logic_vector(DATA_WIN*CHAN_NUM-1 downto 0):=(others=>'0');
begin
	
	pr_mux: process(aclk, reset) is
	begin
		if rising_edge(aclk) then
			if (reset = '0') then
				dmux <= (0 => '1', others => '0');
			elsif (den = '1') then
				dmux <= dmux(DCHAN-2 downto 0) & dmux(DCHAN-1);
			end if;
		end if;
	end process;
	
	pr_sel: process(aclk, reset) is
	begin
		if rising_edge(aclk) then
			if (reset = '0') then
				dsel <= (others => '0');
			else
				dsel <= dmux;
			end if;
		end if;
	end process;	
	ddat <= dmux(DCHAN-1) when rising_edge(aclk);

	xPACK: for ii in 0 to DCHAN-1 generate
		xDATA: for jj in 0 to CHAN_NUM-1 generate
			constant DCUT : integer:=(CHAN_NUM*ii)+jj;
		begin
			pr_dat: process(aclk, reset) is
			begin
				if rising_edge(aclk) then
					if (dsel(ii) = '1') then
						dpack(DATA_WIN*(DCUT+1)-1 downto DATA_WIN*DCUT) <= ddis(DATA_WIN*(jj+1)-1 downto jj*DATA_WIN);
					end if;				
				end if;
			end process;	
		end generate;
	end generate;
	
	xSELECT: for ii in 0 to CHAN_NUM-1 generate
		pr_dat: process(aclk, reset) is
		begin
			if rising_edge(aclk) then
				if (chan(ii) = '1') then
					ddis(DATA_WIN*(ii+1)-1 downto DATA_WIN*ii) <= din(DATA_WIN*(ii+1)-1 downto DATA_WIN*ii);
				else
					ddis(DATA_WIN*(ii+1)-1 downto DATA_WIN*ii) <= (others=>'0');
				end if;
			end if;
		end process;
	end generate;
	dena <= ddat when rising_edge(aclk);
	
	-- pr_chan: process(aclk, reset) is
	-- begin
		-- if rising_edge(aclk) then
			-- if (reset = '0') then
				-- nchan <= (0 => '1', others => '0');
			-- else
				-- nchan <= nchan(CHAN_NUM-2 downto 0) & nchan;
			-- end if;
		-- end if;
	-- end process;	

end generate;
 


end ctrl_repackNxM;