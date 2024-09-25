---------------------------------------------------------------------------------
-- Project Title : 2D FFT-based convolution
-- Author        : Weiwei Wang
-- Create Date   : 03/25/2024
-- Description   : THis designe is 2D FFT-based convolution using FFT2D 
---------------------------------------------------------------------------------
library work;
use work.FFT2D_package.all;

library ieee;
use ieee.std_logic_1164.all;   use ieee.std_logic_arith.all;
use ieee.std_logic_signed.all; use ieee.std_logic_unsigned.all;

package FFT2Cov_package is          -- User defined type
	CONSTANT W,H :INTEGER := 16;     -- Input image size 12*12
	
	SUBTYPE U9 IS INTEGER RANGE 0 TO 2**9-1;
	SUBTYPE S16 IS INTEGER RANGE -2**15 TO 2**15-1;
	SUBTYPE S20 IS INTEGER RANGE -524288 TO 524287;      					-- for Convolutional kernel
	SUBTYPE S32 IS INTEGER RANGE -2147483647 TO 2147483647;
	
	TYPE array_FFT  IS ARRAY (0 TO W-1,0 TO H-1)      OF S20;--S16; 	-- Register for FFT and Input datas
	TYPE array_Krl  IS ARRAY (0 TO W-1,0 TO H-1)      OF S20; 			-- Register for FFT
	
	--CONSTANT krl :INTEGER := 3;      -- Kernel Size 3*3
	--type krl_memory is array (0 to krl-1, 0 to krl-1) of INTEGER;
	--TYPE array_In   IS ARRAY (0 TO W-1,0 TO H-1)      OF S20;--S16; 	-- Register for Input datas
	
	TYPE states IS (start, load, FFT1, FFT2, MUL, IFFT1, IFFT2,done);
	
	COMPONENT FFT2D 						-- 2D FFT transform
	port(
			clk:	   IN STD_LOGIC;		-- Clock cycle
			rst_f2:  IN STD_LOGIC;		-- Reset
			xr,xi:   In S20;--S16;				-- input x 
			IDFT_sel:In STD_LOGIC;
			FFT2D_valid: out std_logic;
			add_2o,mul_2o: out S20;
			fft2r,fft2i: out S20--S16
	);        
	END COMPONENT;
end FFT2Cov_package;

library work;
use work.FFT2D_package.all;

library work;
use work.FFT2Cov_package.all;

library ieee;
use ieee.std_logic_1164.all;   use ieee.std_logic_arith.all;
use ieee.std_logic_signed.all; use ieee.std_logic_unsigned.all;
USE ieee.math_real.ALL;


entity FFT2Conv is
	port(
			clk, rst   : In  std_logic;
			xr    	  : In  S20;--S16;			   	-- Input
			xi			  : In  S20;--S16;
			FFTconv_valid: out std_logic;
			additions  : out S32;
			multipls   : out S32;
			Covr_out   : Out S20; --S16;			   	-- Output
			Covi_out   : out S20--S16			  			-- Output
		
	);
end entity;

architecture FFT2_fpga of FFT2Conv is
------------------Regular Paremeters-----------------------
	SIGNAL state		: states;					-- States of system
	SIGNAL rst_fw		: STD_LOGIC;				-- Reset signal for component function(2D FFT)
	SIGNAL fft2_valid	: STD_LOGIC;				-- fft2d output is ready
	SIGNAL IFFT_sel	: STD_LOGIC;				-- IFFT signal (1: Inverse Fourier transform,0: Forward transform)
	SIGNAL blockr,blocki			: S20;--S16;			-- FFT2D input
	SIGNAL reg_xr, reg_xi		: array_FFT;		-- Reg for input data
	SIGNAL FFT_xr,FFT_xi			: array_FFT;	-- Reg for FFT result of the input image x
	SIGNAL reg_FFTr, reg_FFTi	: array_FFT; 	-- Reg for FFT multiplication result
	SIGNAL reg_IFFTr, reg_IFFTi: array_FFT;	-- Reg for IFFT result
	SIGNAL reg_subout_r, reg_subout_i: S20:= 0;   	-- Output of FFT2D 
	SIGNAL add_2o, mul_2o: S20;
--------------------------Filter-----------------------------
/*	constant krnl: krl_memory := (
				 (1, 2, 1),
				 (2, 1, 2),
				 (1, 2, 1));
*/				 
	constant FFT_kernelr: array_Krl := (
				(13312,11723,7716,3159,0,-760,476,2262,3072,2262,476,-760,0,3159,7716,11723),
				(11723,8548,3972,-0,-1840,-1268,706,2472,2838,1748,292,-0,1840,5532,9590,12089),
				(7716,3972,0,-2524,-2772,-1150,1024,2346,2172,972,0,476,2772,6094,8865,9590),
				(3159,0,-2524,-3365,-2394,-424,1306,1867,1176,0,-541,300,2394,4759,6094,5532),
				(0,-1840,-2772,-2394,-1024,502,1324,1056,0,-1056,-1324,-502,1024,2394,2772,1840),
				(-760,-1268,-1150,-424,502,1069,892,0,-1176,-2036,-2154,-1511,-502,300,476,-0),
				(476,706,1024,1306,1324,892,0,-1141,-2172,-2754,-2721,-2154,-1324,-541,0,292),
				(2262,2472,2346,1867,1056,0,-1141,-2155,-2838,-3048,-2754,-2036,-1056,-0,972,1748),
				(3072,2838,2172,1176,0,-1176,-2172,-2838,-3072,-2838,-2172,-1176,0,1176,2172,2838),
				(2262,1748,972,-0,-1056,-2036,-2754,-3048,-2838,-2155,-1141,0,1056,1867,2346,2472),
				(476,292,0,-541,-1324,-2154,-2721,-2754,-2172,-1141,0,892,1324,1306,1024,706),
				(-760,-0,476,300,-502,-1511,-2154,-2036,-1176,0,892,1069,502,-424,-1150,-1268),
				(0,1840,2772,2394,1024,-502,-1324,-1056,0,1056,1324,502,-1024,-2394,-2772,-1840),
				(3159,5532,6094,4759,2394,300,-541,0,1176,1867,1306,-424,-2394,-3365,-2524,0),
				(7716,9590,8865,6094,2772,476,0,972,2172,2346,1024,-1150,-2772,-2524,0,3972),
				(11723,12089,9590,5532,1840,-0,292,1748,2838,2472,706,-1268,-1840,-0,3972,8548));
	constant FFT_kerneli: array_Krl := (
				(0,-4856,-7716,-7627,-5120,-1834,476,937,0,-937,-476,1834,5120,7627,7716,4856),
				(-4856,-8548,-9590,-7824,-4442,-1268,292,-0,-1176,-1748,-706,1793,4442,5532,3972,-0),
				(-7716,-9590,-8865,-6094,-2772,-476,0,-972,-2172,-2346,-1024,1150,2772,2524,0,-3972),
				(-7627,-7824,-6094,-3365,-992,0,-541,-1867,-2838,-2641,-1306,300,992,0,-2524,-5532),
				(-5120,-4442,-2772,-992,0,-208,-1324,-2550,-3072,-2550,-1324,-208,0,-992,-2772,-4442),
				(-1834,-1268,-476,0,-208,-1069,-2154,-2880,-2838,-2036,-892,-0,208,-300,-1150,-1793),
				(476,292,0,-541,-1324,-2154,-2721,-2754,-2172,-1141,0,892,1324,1306,1024,706),
				(937,0,-972,-1867,-2550,-2880,-2754,-2155,-1176,0,1141,2036,2550,2641,2346,1748),
				(0,-1176,-2172,-2838,-3072,-2838,-2172,-1176,0,1176,2172,2838,3072,2838,2172,1176),
				(-937,-1748,-2346,-2641,-2550,-2036,-1141,0,1176,2155,2754,2880,2550,1867,972,-0),
				(-476,-706,-1024,-1306,-1324,-892,0,1141,2172,2754,2721,2154,1324,541,0,-292),
				(1834,1793,1150,300,-208,0,892,2036,2838,2880,2154,1069,208,-0,476,1268),
				(5120,4442,2772,992,0,208,1324,2550,3072,2550,1324,208,0,992,2772,4442),
				(7627,5532,2524,0,-992,-300,1306,2641,2838,1867,541,-0,992,3365,6094,7824),
				(7716,3972,0,-2524,-2772,-1150,1024,2346,2172,972,0,476,2772,6094,8865,9590),
				(4856,0,-3972,-5532,-4442,-1793,706,1748,1176,0,-292,1268,4442,7824,9590,8548));
	
begin
	PROCESS(clk, rst)
		VARIABLE lr, lc,i,j: U9;
	
		VARIABLE add_total, mul_total: S20;
	BEGIN
	IF rst = '1' THEN 
		reg_xr	<= (OTHERS => (others =>0));		reg_xi<= (OTHERS => (others =>0));
		FFT_xr	<= (OTHERS => (others =>0));		FFT_xi<= (OTHERS => (others =>0));
		reg_FFTr <= (OTHERS => (others =>0));		reg_FFTi<=(OTHERS => (others =>0));
		reg_IFFTr<= (OTHERS => (others =>0));		reg_IFFTi <= (OTHERS => (others =>0));
		rst_fw 	<= '1';
		IFFT_sel <= '0';
		state 	<= start;
	ELSIF rising_edge(clk) THEN
		CASE state IS
		WHEN start =>
			lr := 0; lc := 0;
			i  := 0;  j := 0;	
			add_total := 0;
			mul_total := 0;
			FFTconv_valid <= '0';
			state <= load;
		--------------------------------
		WHEN load =>									-- Load input data 
			if lr < W then
				if lc < H then
					reg_xr(lr,lc) <= xr;
					reg_xi(lr,lc) <= xi;				-- input data did not have imaginary part
					lc := lc + 1;
					if lc = H then
						lc := 0;
						lr:= lr + 1;
						if lr = W then
							lr := 0;
							state <= FFT1;
							rst_fw <= '0';
						end if;
					end if;
				end if;
			end if;			
		--state <= FFT1;		
		--------------------------------
		WHEN FFT1 =>									-- load dada to FFT2D
			if rst_fw = '1' THEN
				rst_fw <= '0';
			else	
				if lr < W then
					if lc < H then
						blockr <= reg_xr(lr,lc); 
						blocki <= reg_xi(lr,lc);	
						lc := lc + 1;
					else
						lc := 0;
						lr := lr +1;
					end if;
				else
					lr := 0;
					state <= FFT2;
				end if;
			end if;
		--------------------------------
		when FFT2 =>									-- calculate FFT2D
			if fft2_valid = '1' then
				if lr < W then	
					if lc < H then     				-- if define a new variable, can pipelining.
						FFT_xr(lr,lc) <= reg_subout_r;
						FFT_xi(lr,lc) <= reg_subout_i;
						lc := lc+1;						
						if lc = H then					
							lc := 0;
							lr := lr +1;
							if lr = W then  			-- All FFT are done
								add_total := add_total + add_2o;
								mul_total := mul_total + mul_2o;
								state <= MUL;
								rst_fw <= '1';
								lr := 0;
							end if;
						end if;
					end if;
				end if;
			end if;			
		--------------------------------
		WHEN MUL =>										-- Multiplication of input image FFT and kernel FFT
			for i in 0 to W-1 loop
				for j in 0 to H-1 loop
					reg_FFTr(i,j) <= (FFT_kernelr(i,j) * FFT_xr(i,j) - FFT_kerneli(i,j) * FFT_xi(i,j))/2**12;
					reg_FFTi(i,j) <= (FFT_kernelr(i,j) * FFT_xi(i,j) + FFT_kerneli(i,j) * FFT_xr(i,j))/2**12; 
					add_total := add_total + 2;
					mul_total := mul_total + 4;
				end loop;
			end loop;

			state <= IFFT1;
			IFFT_sel <= '1';
			rst_fw <= '0';
		--------------------------------
		WHEN IFFT1 =>										-- load data to FFT2D do the IFFT
			if rst_fw = '1' THEN
				rst_fw <= '0';
			else	
				if lc < H then
					if lr < W then
						blockr <= reg_FFTr(lr,lc); 	
						blocki <= reg_FFTi(lr,lc);				
						lr := lr + 1;				
					else 										-- Using If...else... since we need to delay a cycle for each column
							lr := 0;
							lc := lc +1;
					end if;
				else	
					lc := 0;
					state <= IFFT2;		
				end if;
			end if;
		--------------------------------
		when IFFT2 =>										-- calculate FFT2D
			if fft2_valid = '1' then
				if lc < H then	
					if lr < W then     					-- if define a new variable, can pipelining. 
						reg_IFFTr(lr,lc) <= reg_subout_r;
						reg_IFFTi(lr,lc) <= reg_subout_i;
						lr := lr+1;
						if lr = W then						
							lr := 0;
							lc := lc +1;
							if lc = H then  				-- All IFFT are done
								add_total := add_total + add_2o;
								mul_total := mul_total + mul_2o;
								lc := 0;
								state <= done;
								rst_fw <= '1';				-- stop tranform opertions
								IFFT_sel <= '0';	
								FFTconv_valid <= '1';	-- Ready for output	
							end if;
						end if;
					end if;
				end if;
			end if;					
		--------------------------------
		WHEN done =>
			additions <= add_total;
			multipls <= mul_total;
			if FFTconv_valid = '1' then
				if lr < W then	
					if lc < H then     					-- if define a new variable, can pipelining. 
						Covr_out <= reg_IFFTr(lr,lc);
						Covi_out <= reg_IFFTi(lr,lc);
						lc := lc +1;
						if lc = H then						
							lc := 0;
							lr := lr +1;
							if lr = W then  				
								lr := 0;
								state <= start;
							end if;
						end if;
					end if;
				end if;
			end if;	
		END CASE;
	END IF;
	
	END PROCESS;
	
	-------------------- Mapping -------------------
	FFT2D_valid: FFT2D PORT MAP(clk, rst_fw, blockr, blocki,IFFT_sel, fft2_valid, add_2o, mul_2o,reg_subout_r, reg_subout_i);  

end architecture;
