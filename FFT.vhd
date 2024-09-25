---------------------------------------------------------------------
-- File name : FFT.vhd
-- Author:     Weiwei Wang
-- Date  :     03/2024
-- Description:
--					Genertic 8,16,32 points DIF FFT algorithm using a register
--					array for data and coefficients.
--					Can do the IFFT(IDFT:1, Input data should be column by column).

---------------------------------------------------------------------
PACKAGE n_bits_int IS        --Usser defined types
	CONSTANT N   : INTEGER := 16;					-- Number of points 
	CONSTANT ldN : INTEGER := 4;           	-- LOG_2 number of points
	SUBTYPE U9  IS INTEGER RANGE 0 TO 2**9-1;
	SUBTYPE S16 IS INTEGER RANGE -2**15 TO 2**15-1;
	SUBTYPE S20 IS INTEGER RANGE -524288 TO 524287;
	SUBTYPE S32 IS INTEGER RANGE -2147483647 TO 2147483647;
	TYPE ARRAY0_x IS ARRAY (0 TO N-1) OF S20;	--S16; -- For 32-point FFT:S20
	TYPE STATE_TYPE IS (start, load, calc, update, reverse, done);
END n_bits_int;

LIBRARY work;  USE work.n_bits_int.ALL;

LIBRARY ieee;  USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_signed.ALL;
----------------------------------------------------------------------
ENTITY FFT IS                        ------------------------>Interface
	PORT (clk, rst_fft : IN STD_LOGIC;   -- Clock abd reset			
			xr_in, xi_in : IN S20;--S16;     -- Real and img. input
			IDFT         : In std_logic;    	-- Inverse DFT mark(0:DFT, 1:IDFT)
			FFTblock     : OUT std_logic;   	-- FFT output is vali
			add_o, mul_o : OUT S20;
			fftr, ffti   : OUT S20--S16      -- Rel and img. output
	);         
END FFT;
-----------------------------------------------------------------------
ARCHITECTURE fpga OF FFT IS
	SIGNAL rcount_o    : U9:=0 ;         -- Bitreverse index counter
	SIGNAL stage_o     : U9;				  -- Stage count
	SIGNAL gcount_o    : U9;				  -- group count
	SIGNAL i1_o, i2_o  : U9 := 0;     	  -- (Dual) data index
	SIGNAL k1_o, k2_o  : U9 := 0;     	  -- Index offset
	SIGNAL w_o, dw_o   : U9 := 0;        -- Cos/Sin (increment) angle
	SIGNAL  w,  wo     : U9 := 0;		  -- Decision tree location loop FSM
	SIGNAL sin, cos    : S16 := 0;
	
	SIGNAL     s       : STATE_TYPE;     -- State machine variable
	SIGNAL 	xr, xi 	 : ARRAY0_x;     --INput sequence length
	
	-- Sine and cosine coefficient arrays
-----------------------------------------------------------------------
	TYPE array_twiddle8  IS ARRAY (0 TO 3)  OF S16; 
	TYPE array_twiddle16 IS ARRAY (0 TO 7)  OF S16;
	TYPE array_twiddle32 IS ARRAY (0 TO 15) OF S16;
	CONSTANT sin_K8:   array_twiddle8:= (0, 11585, 16383, 11585); 
	CONSTANT cos_K8:   array_twiddle8:= (16383, 11585, 0, -11585);
	
	CONSTANT sin_K16:  array_twiddle16:= (0, 6272, 11584, 15168, 16383, 15168, 11584, 6272); 
	CONSTANT cos_K16:  array_twiddle16:= (16383, 15168, 11584, 6272, 0, -6272, -11584, -15168);

	CONSTANT sin_K32:  array_twiddle32:= (0, 3196, 6270, 9102, 11585, 13622, 15136, 16068, 16383, 16068, 15136, 13622, 11585, 9102, 6270, 3196); 
   CONSTANT cos_k32:  array_twiddle32:= (16383, 16068, 15136, 13622, 11585, 9102, 6270, 3196, 0, -3196, -6270, -9102, -11585, -13622, -15136, -16068);
	
BEGIN
	
Twiddle_load: PROCESS(clk) -----------> Read sin and cos from ROM
    BEGIN
      IF falling_edge(clk) THEN
        CASE N IS
		  WHEN 8 =>
			 sin <= sin_K8(w);
			 cos <= cos_K8(w);
		  WHEN 16 =>
			 sin <= sin_K16(w);
			 cos <= cos_K16(w);
		  WHEN 32 =>
			 sin <= sin_K32(w);
			 cos <= cos_K32(w);	
		  WHEN OTHERS =>
			 REPORT "Error." SEVERITY FAILURE;
        END CASE;
      END IF;
  END PROCESS Twiddle_load;	
	
	State: PROCESS(clk, rst_fft, w)   ------> FFT inbehavioral style
		VARIABLE i1,  i2,  gcount, k1, k2 : U9 := 0;
		VARIABLE stage, dw, count, rcount : U9 := 0;
		VARIABLE tr,  ti : S20 := 0;
		VARIABLE slv, rslv : STD_LOGIC_VECTOR(0 TO ldN-1);
		VARIABLE add, mul: S20; -- Counter of ADD and MUL
	BEGIN
		IF rst_fft = '1' THEN          ------> Asynchronous reset
			s <= start;
			FFTblock <= '0';
			xr <= (others=>0); xi<= (others=>0);
		ELSIF rising_edge(clk) THEN
			CASE s IS                	 ------> Next State assignments
			WHEN start=>
				FFTblock <= '0';
				xr <= (others=>0); xi<= (others=>0);
				s <= load; count := 0;
				gcount := 0; stage := 1; i1 := 0;  i2 := N/2; K1 := N;
				k2 := N/2; dw := 1; w <= 0;
				add := 0; mul := 0;
			WHEN load =>             	 ------> Read in all data from I/O ports
				xr(count) <= xr_in;  xi(count) <= xi_in;
				count := count + 1;
				IF count = N THEN s <= calc;
				ELSE              s <= load;
				END IF;
			WHEN calc =>             	 ------> Do the butterfly computaion
				tr := xr(i1) - xr(i2);
				xr(i1) <= xr(i1) + xr(i2);
				ti := xi(i1) -xi(i2);
				xi(i1) <= xi(i1) + xi(i2);
				add := add + 4;
				if IDFT then
					xr(i2) <= (cos * tr - sin * ti)/2**14;
					xi(i2) <= (cos * ti + sin * tr)/2**14;
				else
					xr(i2) <= (cos * tr + sin * ti)/2**14;
					xi(i2) <= (cos * ti - sin * tr)/2**14;
				end if;
				add := add + 2;
				mul := mul + 4;
				s <= update;
			WHEN update =>           	-------> All counters and points
				s <= calc;            	-- By default do next butterfly
				i1 := i1 + k1;        	-- Next butterfly in group
				i2 := i1 + k2;
				wo <= 1;
				IF i1 >= N-1 THEN     	-------> All butterfliers done in group?
					gcount := gcount + 1;
					i1 := gcount;
					i2 := i1 + k2;
					wo <= 2;
					IF gcount >= k2 THEN -------> All groups done in stages?
						gcount := 0; i1 := 0; i2 := k2;
						dw := dw *2;
						stage := stage + 1;
						wo <= 3;
						IF stage > ldN THEN -----> All stages done
							s <= reverse;
							count := 0;
							wo <= 4;
						ELSE                -----> Start new stage
							k1 := k2;  k2 := k2/2;
							i1 := 0;   i2 := k2;
							w  <= 0;
							wo <= 5;
						END IF;
					ELSE     				-------> Start new group
						i1 := gcount; i2 := i1 + k2;
						w  <= w +dw;
						wo <= 6;
					END IF;
				END IF;
			WHEN reverse =>    			-------> Apply bitreverse
				FFTblock <= '1';			-- fft_valid--> FFTblock
				slv := CONV_STD_LOGIC_VECTOR(count, ldn);
				FOR i IN 0 TO ldn-1 LOOP
					rslv(i) := slv(ldn -i - 1);
				END LOOP;
				rcount := CONV_INTEGER('0' & rslv);
				fftr <= xr(rcount);  ffti <= xi(rcount);
				count := count + 1;
				IF count >= N THEN 
					s <= done;
					
				ELSE               
					s <= reverse;
				END IF;
			WHEN done =>             	-------> Output of results
				s <= start;           	-- Start next cycle
				FFTblock <= '0';
			END CASE;
		END IF;
		i1_o <= i1;   						-------> Provide some test signals as outputs
		i2_o <= i2;
		stage_o <= stage;
		gcount_o <= gcount;
		k1_o <= k1;
		k2_o <= k2;
		w_o <= w;
		dw_o <= dw;
		rcount_o <= rcount;
		add_o <= add;
		mul_o <= mul;
	END PROCESS State;
	/*
	Rk : FOR k IN 0 TO 7 GENERATE   -- Show first 8
		xr_out(k) <= xr(k);          -- register values
		xi_out(k) <= xi(k);
	END GENERATE;
	*/
END fpga;

	