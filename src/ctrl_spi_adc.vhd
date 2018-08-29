-------------------------------------------------------------------------------
--
-- Title		: ctrl_spi_adc.vhd
-- Author		: Alexander Kapitanov
-- Company		: ...
-- E-mail		: sallador@bk.ru
--
-- Version		: 1.0	
--
-------------------------------------------------------------------------------
--
-- Description : 	Модуль SPI host MASTER: для микросхемы (HOLT-6131)  
--			Особенность: работает в half_duplex, т.е. либо чтение, либо запись.
--			Сначала передается 1 байт команды, затем 2 байта данных в формате (15:0) => (7:0)(15:8)
--			На HI-6131 установлен режим BENDI = 0 (little endian), RAM EEC disabled.
--			Данные делятся на регистровое поле и на поток команд MIL-1553.
--
-------------------------------------------------------------------------------
--
--  Version 1.1   13.03.2013
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package ctrl_spi_adc_pkg is
	component ctrl_spi_adc is
		port(
			clk			: in  std_logic; --! Data clock	 
			-- sys_clk		: in  std_logic; --! System clock
			reset		: in  std_logic; --! Global reset

			mg_addr		: in  std_logic_vector(07 downto 0); --! Host address
			mg_data_i	: in  std_logic_vector(15 downto 0); --! Host data (in)
			mg_data_o	: out std_logic_vector(15 downto 0); --! Host data (out)
			mg_data_v	: out std_logic; --! Host data valid to out
			mg_start	: in  std_logic; --! Host data start transaction 
			mg_ready	: out std_logic; --! Host ready to send/receive data	
			
			spi_do		: out std_logic; --! SPI: Data out
			spi_di		: in  std_logic; --! SPI: Data in
			spi_ck		: out std_logic; --! SPI: Clock data
			spi_cs		: out std_logic	 --! SPI: Chip select (enable)
		);
	end component;
end package;

library ieee;
use ieee.std_logic_1164.all; 
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all; 

use work.ctrl_clk_prescaller_pkg.all;

entity ctrl_spi_adc is
	port(
		clk			: in  std_logic; --! Data clock	 
		-- sys_clk		: in  std_logic; --! System clock
		reset		: in  std_logic; --! Global reset

		mg_addr		: in  std_logic_vector(07 downto 0); --! Host address
		mg_data_i	: in  std_logic_vector(15 downto 0); --! Host data (in)
		mg_data_o	: out std_logic_vector(15 downto 0); --! Host data (out)
		mg_data_v	: out std_logic; --! Host data valid to out
		mg_start	: in  std_logic; --! Host data start transaction 
		mg_ready	: out std_logic; --! Host ready to send/receive data	
		
		spi_do		: out std_logic; --! SPI: Data out
		spi_di		: in  std_logic; --! SPI: Data in
		spi_ck		: out std_logic; --! SPI: Clock data
		spi_cs		: out std_logic	 --! SPI: Chip select (enable)	
	);
end ctrl_spi_adc;

architecture ctrl_spi_adc of ctrl_spi_adc is

signal 	clkz0, clkz1, clkz2, clkz3		: std_logic;
signal	z2z3, z3z2						: std_logic;
signal	rstp							: std_logic;

signal	ready							: std_logic;	   

signal	cnt				: std_logic_vector(05 downto 00);
signal	reg_out			: std_logic_vector(23 downto 00);
signal	reg_int			: std_logic_vector(23 downto 00);
signal	data_i			: std_logic;

type	stp_type is ( s0, s1, s2, s3, s4, s5, s6, s7 );
signal	stp				: stp_type;

signal	sclk_s			: std_logic;
signal	sdata			: std_logic;
signal	csn				: std_logic;
signal	clk_c			: std_logic;  

signal	data_o			: std_logic_vector(15 downto 00);
signal	data_v			: std_logic;
signal	data_z			: std_logic;

begin  
		
xCLK_DIVIDE : ctrl_clk_prescaller
	generic map(
		CLKIN  => 200000,
		CLKOUT => 010000	
	)
	port map(
		clk_in  => clk,
		clk_out => clk_c
	);
	
clkz0 <= clk_c when rising_edge(clk); 
clkz1 <= clkz0 when rising_edge(clk); 
clkz2 <= clkz1 when rising_edge(clk); 
clkz3 <= clkz2 when rising_edge(clk); 

rstp <= not reset when rising_edge(clk);

z2z3 <= (not clkz2) and clkz3 when rising_edge(clk); 
z3z2 <= (not clkz3) and clkz2 when rising_edge(clk); 

pr_stp: process(clk) is
begin
	if (rising_edge(clk)) then		
		case (stp) is	
			when s0 =>	  
				sclk_s 	<= '0';	-- по переднему фронту
				ready 	<= '1';
				csn  	<= '1';
				if (mg_start = '1') then
					stp <= s1 ;
				end if;
			when s1 =>
				ready <= '0' ;
				if (z2z3 = '1') then 
					stp <= s2;
				end if;
			when s2 =>
				if (z2z3 = '1') then 
					stp <= s3;
					csn <= '0';
				end if;		 
			when s3 =>			 
				if (z3z2 = '1') then 
					sclk_s <= not sclk_s;
					stp <= s4;
				end if;		 
			when s4 =>			 
				if (z2z3 = '1') then 
					sclk_s <= not sclk_s;
					stp <= s5;
				end if;		 
			when s5 =>	  
				if (cnt(5) = '1') then 
					stp <= s6; 
				else
					stp <= s3;
				end if;							
			when s6 =>
				if (z3z2 = '1') then 
					csn <= '1';
					stp <= s7; 
				end if;		 
			when s7 =>
				if (z2z3 = '1') then 
					stp <= s0; 
				end if;					
		end case;
		if (rstp = '1') then
			 stp <= s0;
		end if;
	end if;
end process;

pr_reg_out: process(clk) is
begin
	if (rising_edge(clk)) then
		if (stp = s1) then	 
			reg_out(07 downto 00) <= mg_data_i(15 downto 08);-- DATA	
			reg_out(15 downto 08) <= mg_data_i(07 downto 00);-- DATA				
		   	reg_out(23 downto 16) <= mg_addr(07 downto 00);	-- ADDR	
			   
			cnt <= "001001";
		elsif (stp = s5) then
			reg_int	<= reg_int( 22 downto 0 ) & data_i;
			reg_out <= reg_out( 22 downto 0 ) & '0';		 
			cnt <= cnt + 1;
		end if;
		if (stp = s1) then	 
			data_o <= (others => '0');
			data_v <= '0';
		elsif (stp = s6) then
			data_o <= reg_int(15 downto 0);
			data_v <= '1';
		end if;		
	end if;
end process;

data_z <= data_v when rising_edge(clk);
mg_data_v <= (data_v and not data_z) when rising_edge(clk);
mg_data_o(15 downto 08) <= data_o(07 downto 00) when rising_edge(clk);	
mg_data_o(07 downto 00) <= data_o(15 downto 08) when rising_edge(clk);
sdata <= reg_out(23);

spi_ck <= sclk_s	when rising_edge(clk);
spi_do <= sdata		when rising_edge(clk);
spi_cs <= csn		when rising_edge(clk);
data_i <= spi_di	when rising_edge(clk); 

mg_ready <= ready when rising_edge(clk);

end ctrl_spi_adc;
