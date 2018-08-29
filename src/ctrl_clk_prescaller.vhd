-------------------------------------------------------------------------------
--
-- Title       : ctrl_clk_prescaller
-- Author      : Dmitry Smekhov
-- Company     : Instrumantal Systems
-- E-mail      : dsmv@insys.ru
--
-- Version     : 1.0
--
-------------------------------------------------------------------------------
--
-- Description : Узел предварительного деления частоты
--				 Частота получается не больше заданной, может быть меньше
--
--				 Создан на основе делителя частоты в тетраде trd_ddr2_sodimm_m3
--
--
-------------------------------------------------------------------------------
--
--  Version 1.0  24.02.2011
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package	ctrl_clk_prescaller_pkg is

component ctrl_clk_prescaller is	   
	generic(
		CLKIN		: in integer;	-- значение входной тактовой частоты
		CLKOUT		: in integer	-- значение выходной тактовой частоты
	);
	port(
		clk_in		: in std_logic;	 -- входная тактовая частота   
		clk_out		: out std_logic	 -- выходная тактовая частота  
		
	);
end component;

end package;



library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

library unisim;
use unisim.vcomponents.all;


entity ctrl_clk_prescaller is	   
	generic(
		CLKIN		: in integer;	-- значение входной тактовой частоты
		CLKOUT		: in integer	-- значение выходной тактовой частоты
	);
	port(
		clk_in		: in std_logic;	 -- входная тактовая частота   
		clk_out		: out std_logic	 -- выходная тактовая частота  
		
	);
end ctrl_clk_prescaller;


architecture ctrl_clk_prescaller of ctrl_clk_prescaller is

type prescaller_params_type is record
	DIV 			: std_logic_vector (3 downto 0);
	STAGES 			: integer;
end record;	
	
function set_prescaller_params ( CLKIN_KHZ, CLKOUT_KHZ	: in integer) return  prescaller_params_type is
	variable prescaller_params	: prescaller_params_type:=(DIV=>(others=>'0'), STAGES=>1);
	variable sub 		: integer;
begin
lp_M:	for M in 0 to 10 loop
lp_N:		for N in 0 to 15 loop
				sub := CLKIN_KHZ - CLKOUT_KHZ*(32**M)*2*(N+1);
				if (sub < 0) then
					prescaller_params.DIV := conv_std_logic_vector(N, 4);
					prescaller_params.STAGES := M; 
					exit lp_M;
				end if;
			end loop lp_N;
		end loop lp_M;
return prescaller_params;

end set_prescaller_params;	

constant  prescaller_params	: prescaller_params_type:=set_prescaller_params( CLKIN, CLKOUT);		

--*******************************************
signal prescaller_in 		: std_logic_vector(prescaller_params.STAGES downto 0);	
signal prescaller_out 		: std_logic_vector(prescaller_params.STAGES downto 0);	


begin

x_prescaller0: SRL16 
	port map (
    	Q   => prescaller_out(0),
        A0  => prescaller_params.DIV(0),
        A1  => prescaller_params.DIV(1),
        A2  => prescaller_params.DIV(2),
        A3  => prescaller_params.DIV(3),
        CLK => clk_in,
        D   => prescaller_in(0)
       ); 
	   
prescaller_in(0) <= prescaller_out(0) xor '1';	

gn_prescaller : if (prescaller_params.STAGES/=0) generate
	gn_stages : for i in 1 to prescaller_params.STAGES generate

	x_prescaller: SRL16 
	  port map (
	        Q   => prescaller_out(i),
	        
	        A0  => '1',
	        A1  => '1',
	        A2  => '1',
	        A3  => '1',
	        CLK => prescaller_in(i-1),
	        D   => prescaller_in(i)
	       ); 
		   
		prescaller_in(i) <= prescaller_out(i) xor '1';
	end generate;
end generate;

clk_out <= prescaller_in(prescaller_params.STAGES);

end ctrl_clk_prescaller;
