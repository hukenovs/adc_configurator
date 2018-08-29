-------------------------------------------------------------------------------
--
-- Title		: rtl_gray_count.vhd
-- Author		: Alexander Kapitanov
-- Company		: ...
-- E-mail		: sallador@bk.ru
--
-- Version		: 1.0	
--
-------------------------------------------------------------------------------
--
-- Description :  Gray counter for FIFO sync	 
--		
--				G[N-1] = B[N-1] 
--				G[i] = b[i+1] xor b[i] 
--
-------------------------------------------------------------------------------
--
--  Version 1.0   01.06.2013
--
-------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
    
entity rtl_gray_count is
    generic (
		COUNTER_WIDTH :	integer:=4
    );
    port (                                  
        cnt		: out std_logic_vector(COUNTER_WIDTH-1 downto 0);  
        ena		: in  std_logic; --! Count enable
        rst		: in  std_logic; --! Count reset
        clk		: in  std_logic  --! Input clock
    );
end entity;

architecture rtl_gray_count of rtl_gray_count is
    signal cnt_bin : std_logic_vector(COUNTER_WIDTH-1 downto 0);
begin

pr_gray: process (clk) begin
    if (rising_edge(clk)) then
        if (rst = '1') then
            cnt_bin <= conv_std_logic_vector(1, COUNTER_WIDTH);  
            cnt <= (others => '0');
        elsif (ena = '1') then
            cnt_bin <= cnt_bin + 1;
			
			cnt(COUNTER_WIDTH-1) <= cnt_bin(COUNTER_WIDTH-1);
			cnt(COUNTER_WIDTH-2 downto 0) <= cnt_bin(COUNTER_WIDTH-1 downto 1) xor cnt_bin(COUNTER_WIDTH-2 downto 0);
			-- for i in 0 to COUNTER_WIDTH-2 loop:
				-- cnt(i) <= cnt_bin(i+1) xor cnt_bin(i);
			-- end loop;			
			
			
            -- cnt <= (cnt_bin(COUNTER_WIDTH-1) & cnt_bin(COUNTER_WIDTH-2 downto 0) xor cnt_bin(COUNTER_WIDTH-1 downto 0));
        end if;
    end if;
end process;
   
end rtl_gray_count;