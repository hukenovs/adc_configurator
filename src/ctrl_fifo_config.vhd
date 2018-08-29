-------------------------------------------------------------------------------
--
-- Title		: ctrl_fifo_config.vhd
-- Author		: Alexander Kapitanov
-- Company		: ...
-- E-mail		: sallador@bk.ru
--
-- Version		: 1.0	
--
-------------------------------------------------------------------------------
--
-- Description :  FIFO with Gray counters and indep. clocks (async) 
--				 
-------------------------------------------------------------------------------
--
--  Version 1.0   03.06.2013
--
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package ctrl_fifo_config_pkg is  
	component ctrl_fifo_config is
		generic (
			DATA_WIDTH		: integer:= 8; --! Data width
			ADDR_WIDTH		: integer:= 4 --! Address width = log2(FIFO_DEPTH)
		);
		port (
			---- global clear ----	       
			reset			: in  std_logic; --! Reset FIFO        
			---- clocks ----		       
			wr_clk          : in  std_logic; --! Write clock		
			rd_clk			: in  std_logic; --! Read clock		
			---- data ----		
			data_i			: in  std_logic_vector(DATA_WIDTH-1 downto 0); --! Data input
			data_o			: out std_logic_vector(DATA_WIDTH-1 downto 0); --! Data output	
			---- wr/rd ----
			rd_en			: in  std_logic; --! Read enable
			wr_en			: in  std_logic; --! Write enable		
			---- flags ----
			empty			: out std_logic; --! Empty fifo
			full			: out std_logic --! Full fifo
		);
	end component;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
    
entity ctrl_fifo_config is
    generic (
		DATA_WIDTH		: integer:= 8; --! Data width
		ADDR_WIDTH		: integer:= 4 --! Address width = log2(FIFO_DEPTH)
    );
    port (
		---- global clear ----	       
		reset			: in  std_logic; --! Reset FIFO        
		---- clocks ----		       
		wr_clk          : in  std_logic; --! Write clock		
        rd_clk			: in  std_logic; --! Read clock		
		---- data ----		
        data_i			: in  std_logic_vector(DATA_WIDTH-1 downto 0); --! Data input
        data_o			: out std_logic_vector(DATA_WIDTH-1 downto 0); --! Data output	
		---- wr/rd ----
        rd_en			: in  std_logic; --! Read enable
        wr_en			: in  std_logic; --! Write enable		
		---- flags ----
        empty			: out std_logic; --! Empty fifo
        full			: out std_logic --! Full fifo
    );
end entity;

architecture ctrl_fifo_config of ctrl_fifo_config is

---- Internal connections & variables ----
constant FIFO_DEPTH			: integer:=2**ADDR_WIDTH;

type RAM is array (integer range <>) of std_logic_vector(DATA_WIDTH-1 downto 0);
signal Mem : RAM (0 to FIFO_DEPTH-1);

signal pNextWordToWrite     : std_logic_vector(ADDR_WIDTH-1 downto 0);
signal pNextWordToRead      : std_logic_vector(ADDR_WIDTH-1 downto 0);
signal EqualAddresses       : std_logic;
signal NextWriteAddressEn   : std_logic;
signal NextReadAddressEn    : std_logic;
signal Set_Status           : std_logic;
signal Rst_Status           : std_logic;
signal Status               : std_logic;
signal PresetFull           : std_logic;
signal PresetEmpty          : std_logic;

signal xempty, xfull        : std_logic;
    
---- Gray counter ----	
component rtl_gray_count is
    generic (
		COUNTER_WIDTH :	integer:=4
    );
    port (                                  
        cnt		: out std_logic_vector(COUNTER_WIDTH-1 downto 0);  
        ena		: in  std_logic; --! Count enable
        rst		: in  std_logic; --! Count reset
        clk		: in  std_logic  --! Input clock
    );
end component;	

begin

---- Read data ----
pr_rd: process(rd_clk) begin
    if (rising_edge(rd_clk)) then
        if (rd_en = '1' and xempty = '0') then
            data_o <= Mem(conv_integer(pNextWordToRead));
        end if;
    end if;
end process;
        
---- Write data ----
pr_wr: process (wr_clk) begin
    if (rising_edge(wr_clk)) then
        if (wr_en = '1' and xfull = '0') then
            Mem(conv_integer(pNextWordToWrite)) <= data_i;
        end if;
    end if;
end process;

---- Gray counters ----
xGray_WR : rtl_gray_count
	generic map ( COUNTER_WIDTH => ADDR_WIDTH )
	port map (
		cnt		=> pNextWordToWrite,
		ena		=> NextWriteAddressEn,
		rst		=> reset,
		clk		=> wr_clk
	);
   
xGray_RD : rtl_gray_count
	generic map ( COUNTER_WIDTH => ADDR_WIDTH )	
	port map (
		cnt		=> pNextWordToRead,
		ena		=> NextReadAddressEn,
		rst		=> reset,
		clk		=> rd_clk
	);

---- R/W Address ----
NextWriteAddressEn <= wr_en and (not xfull);
NextReadAddressEn  <= rd_en and (not xempty);
---- Equal Address ----
EqualAddresses <= '1' when (pNextWordToWrite = pNextWordToRead) else '0';

---- Quadrant selectors ----
process (pNextWordToWrite, pNextWordToRead)
    variable set_status_bit0 :std_logic;
    variable set_status_bit1 :std_logic;
    variable rst_status_bit0 :std_logic;
    variable rst_status_bit1 :std_logic;
begin
    set_status_bit0 := pNextWordToWrite(ADDR_WIDTH-2) xnor pNextWordToRead(ADDR_WIDTH-1);
    set_status_bit1 := pNextWordToWrite(ADDR_WIDTH-1) xor  pNextWordToRead(ADDR_WIDTH-2);
    Set_Status <= set_status_bit0 and set_status_bit1;
    
    rst_status_bit0 := pNextWordToWrite(ADDR_WIDTH-2) xor  pNextWordToRead(ADDR_WIDTH-1);
    rst_status_bit1 := pNextWordToWrite(ADDR_WIDTH-1) xnor pNextWordToRead(ADDR_WIDTH-2);
    Rst_Status      <= rst_status_bit0 and rst_status_bit1;
end process;
    
---- Status latch ----
process (Set_Status, Rst_Status, reset) 
begin
    if (Rst_Status = '1' or reset = '1') then
        Status <= '0'; -- Empty
    elsif (Set_Status = '1') then
        Status <= '1'; -- Full
    end if;
end process;
    
---- Full ----
PresetFull <= Status and EqualAddresses;
    
process (wr_clk, PresetFull) begin
    if (PresetFull = '1') then
        xfull <= '1';
    elsif (rising_edge(wr_clk)) then
        xfull <= '0';
    end if;
end process;
full <= xfull;
    
---- Empty ----
PresetEmpty <= not Status and EqualAddresses; 
    
process (rd_clk, PresetEmpty) begin 
    if (PresetEmpty = '1') then
        xempty <= '1';
    elsif (rising_edge(rd_clk)) then
        xempty <= '0';
    end if;
end process;
empty <= xempty;

end ctrl_fifo_config;