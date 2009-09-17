--   ----------------------------------------------------------------------
--   DspUnit : Advanced So(P)C Sequential Signal Processor
--   Copyright (C) 2007-2009 by Adrien LELONG (www.lelongdunet.com)
--
--   This program is free software; you can redistribute it and/or modify
--   it under the terms of the GNU General Public License as published by
--   the Free Software Foundation; either version 2 of the License, or
--   (at your option) any later version.
--
--   This program is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU General Public License for more details.
--
--   You should have received a copy of the GNU General Public License
--   along with this program; if not, write to the
--   Free Software Foundation, Inc.,
--   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
--   ----------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dspunit_pac.all;
use work.dspalu_pac.all;
use work.Bit_Manipulation.all;
-------------------------------------------------------------------------------
entity fft is
  port (
    --@inputs
	 clk                      : in std_logic;
	 op_en                    : in std_logic;
	 data_in_m0               : in std_logic_vector((sig_width - 1) downto 0);
	 data_in_m2               : in std_logic_vector((sig_width - 1) downto 0);
	 length_reg               : in std_logic_vector((cmdreg_width -1 ) downto 0);
         shift_flags_reg          : in std_logic_vector((cmdreg_width - 1) downto 0);
	 opflag_select            : in std_logic_vector((opflag_width - 1) downto 0);
	 result1                    : in std_logic_vector(sig_width downto 0);
	 result2                    : in std_logic_vector(sig_width downto 0);
	 lut_out                    : in std_logic_vector((lut_out_width - 1) downto 0);
    --@outputs;
	 dsp_bus                  : out t_dsp_bus
       );
end fft;
--=----------------------------------------------------------------------------
architecture archi_fft of fft is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
    constant c_addr_pipe_depth         : integer := 13;
    constant c_ind_width               : integer := cmdreg_width - 2;
    constant c_shiftflag_pipe_depth    : integer := 10;
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_dsp_bus           : t_dsp_bus;
  type t_fft_state is (st_init, st_startpipe, st_performft, st_copy);
  type t_datastate is (st_data_y1, st_data_y2, st_data_u1, st_data_u2);
  signal s_state             : t_fft_state;
  signal s_length_moins            : unsigned((cmdreg_width - 1) downto 0);
  signal s_data_y1_r         : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_y2_r         : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_u1_r         : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_y1         : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_y2         : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_u1         : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_u2         : std_logic_vector((sig_width - 1) downto 0);
  signal s_out_y2_r         : std_logic_vector((sig_width - 1) downto 0);
  signal s_out_u1_r         : std_logic_vector((sig_width - 1) downto 0);
  signal s_out_u2_r         : std_logic_vector((sig_width - 1) downto 0);
  signal s_out_y1         : std_logic_vector((sig_width - 1) downto 0);
  signal s_out_y2         : std_logic_vector((sig_width - 1) downto 0);
  signal s_out_u1         : std_logic_vector((sig_width - 1) downto 0);
  signal s_out_u2         : std_logic_vector((sig_width - 1) downto 0);
  signal s_datastate       : t_datastate;
  signal s_datastate_n1       : t_datastate;
  signal s_length     : unsigned((cmdreg_width - 1) downto 0);
  signal s_radix_count     : unsigned((c_ind_width - 1) downto 0);
  signal s_radix_half    : unsigned((c_ind_width - 1) downto 0);
  type t_addr_pipe is array(0 to c_addr_pipe_depth - 1) of unsigned((cmdreg_width - 1) downto 0);
  type t_wr_pipe is array(0 to c_addr_pipe_depth - 1) of std_logic;
  signal s_addr_pipe         : t_addr_pipe;
  signal s_wr_pipe           : t_wr_pipe;
  signal s_next_index        : unsigned((c_ind_width - 1) downto 0);
  signal s_next_group        : unsigned((c_ind_width - 1) downto 0);
  signal s_butter_index      : unsigned((c_ind_width - 1) downto 0);
  signal s_butter_group      : unsigned((c_ind_width - 1) downto 0);
  signal s_butter_offset     : unsigned((c_ind_width - 1) downto 0);
  signal s_sample_index      : unsigned((c_ind_width - 1) downto 0);
  signal s_sample_index_rev  : unsigned((c_ind_width - 1) downto 0);
  signal s_imag_part         : std_logic;
  signal s_addr_r_m0_tmp     : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_r_m0_tmp2     : unsigned((cmdreg_width - 1) downto 0);
  signal s_radix_count_down  : unsigned((angle_width - 1) downto 0);
  signal s_angle             : unsigned((angle_width - 1) downto 0);
  signal s_angle_offset       : unsigned((angle_width - 1) downto 0);
  signal s_omega1            : unsigned((sig_width - 1) downto 0);
  signal s_omega2            : unsigned((sig_width - 1) downto 0);
  signal s_wr_pipe_in        : std_logic;
  signal s_shift_pipe        : std_logic_vector((c_shiftflag_pipe_depth - 1) downto 0);
  signal s_shift_flags_reg   : std_logic_vector((cmdreg_width - 1) downto 0);
  signal s_result1_shift     : std_logic_vector((sig_width - 1) downto 0);
  signal s_result2_shift     : std_logic_vector((sig_width - 1) downto 0);
  signal s_end_ft            : std_logic;
  signal s_angle_total       : std_logic_vector((angle_width - 1) downto 0);
begin  -- archs_fft
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  p_fft : process (clk)
  begin -- process p_fft
    if rising_edge(clk) then  -- rising clock edge
      if op_en = '0' then
        s_state <= st_init;
	--s_dsp_bus <= c_dsp_bus_init;
        s_dsp_bus.op_done		   <= '0';
        -- memory 0
--        s_dsp_bus.data_out_m0          <= (others => '0');
        -- s_dsp_bus.addr_r_m0            <= (others => '0');
        -- s_dsp_bus.addr_w_m0            <= (others => '0');
        -- s_dsp_bus.wr_en_m0             <= '0';
        --s_dsp_bus.c_en_m0              <= '0';
        -- memory 1
--        s_dsp_bus.data_out_m1          <= (others => '0');
        s_dsp_bus.wr_en_m1             <= '0';
        --s_dsp_bus.c_en_m1              <= '0';
        -- memory 2
--        s_dsp_bus.data_out_m2          <= (others => '0');
        s_dsp_bus.addr_m2              <= (others => '0');
        s_dsp_bus.wr_en_m2             <= '0';
	s_wr_pipe_in <= '0';
      -------------------------------------------------------------------------------
      -- operation management
      -------------------------------------------------------------------------------
      else
        case s_state is
	  when st_init =>
--	    s_count <= 0;
	    if s_dsp_bus.op_done = '0' then
	      s_state <= st_performft;
	    end if;
--	  when st_startpipe =>
--	    -- wait 2 stages for preparing reading data
--	    -- note : the result of the first butterfly (4 stages)
--	      -- will not be written in memory (wait 10 stages)
--	    s_count <= s_count + 1;
--	    if(s_count = 1) then
--	      s_state <= st_performft;
--	    end if;
	    s_wr_pipe_in <= '0';
	  when st_performft =>
	    -- In this state : reading, computing butterfly and writting
	      -- are done concurently
	    -- if s_radix_half > s_length then
--	    if s_radix_count > s_length then
	    if s_end_ft = '1' then
	      --s_dsp_bus.wr_en_m1 <= '1';
	      s_state <= st_copy;
	    end if;
	    s_wr_pipe_in <= '1';
	  when st_copy =>
	    s_wr_pipe_in <= '0';
	    -- write last words to memory
--	    s_count <= s_count + 1;
--	    if(s_count = 10) then
	    if(s_dsp_bus.wr_en_m0 = '0') then
	      s_state <= st_init;
	      s_dsp_bus.op_done <= '1';
	    end if;
	  when others => null;
	end case;
      end if;
    end if;
  end process p_fft;
  -------------------------------------------------------------------------------
  -- Data states
  -------------------------------------------------------------------------------
  p_data : process (clk)
  begin -- process p_data
    if rising_edge(clk) then  -- rising clock edge
      if s_state = st_init then
	  -- initial state is calculated as a function of pipeline depth
--	  s_datastate <= st_data_u1;
	  s_datastate <= st_data_y2;
      else
	  case s_datastate is
	      when st_data_y1 =>
		  s_datastate <= st_data_y2;
	      when st_data_y2 =>
		  s_datastate <= st_data_u1;
	      when st_data_u1 =>
		  s_datastate <= st_data_u2;
	      when others =>
		  s_datastate <= st_data_y1;
	  end case;
      end if;
    end if;
  end process p_data;
  -------------------------------------------------------------------------------
  -- load data (for the next butterfly computation) from memory
  -------------------------------------------------------------------------------
  p_dataload : process (clk)
  begin -- process p_dataload
    if rising_edge(clk) then  -- rising clock edge
      case s_datastate is
        when st_data_y1 =>
	  s_data_y1_r <= data_in_m0;
        when st_data_y2 =>
	  s_data_y2_r <= data_in_m0;

	  -- load sinus
	  s_omega2 <= unsigned(lut_out);
        when st_data_u1 =>
	  s_data_u1_r <= data_in_m0;
	  -- load cosinus
	  s_omega1 <= unsigned(lut_out);
        when st_data_u2 =>
	  s_data_u2 <= data_in_m0;
	  s_data_u1 <= s_data_u1_r;
	  s_data_y2 <= s_data_y2_r;
	  s_data_y1 <= s_data_y1_r;
      end case;
    end if;
  end process p_dataload;
  -------------------------------------------------------------------------------
  -- store data to memory (previous butterfly)
  -------------------------------------------------------------------------------
  p_datastore : process (clk)
  begin -- process p_datastore
    if rising_edge(clk) then  -- rising clock edge
      case s_datastate_n1 is
--      case s_datastate is
        when st_data_y1 =>
--	  s_dsp_bus.data_out_m0 <= s_out_y1;
--	  s_out_y2_r <= s_out_y2;
          s_dsp_bus.data_out_m0 <= s_result1_shift;
	  s_out_y2_r <= s_result2_shift;
        when st_data_y2 =>
	  s_dsp_bus.data_out_m0 <= s_out_y2_r;
        when st_data_u1 =>
	  -- s_out_u1_r <= s_out_u1;
	  s_out_u2_r <= s_out_u2;
	  s_dsp_bus.data_out_m0 <= s_out_u1;
        when st_data_u2 =>
	  s_dsp_bus.data_out_m0 <= s_out_u2_r;
      end case;
    end if;
  end process p_datastore;
  -------------------------------------------------------------------------------
  -- compute the butterfly
  -------------------------------------------------------------------------------
  p_butterfly_reg : process (clk)
  begin -- process p_butterfly_reg
    if rising_edge(clk) then  -- rising clock edge
      if(s_state = st_init) then
	s_out_y1 <= (others => '0');
	s_out_y2 <= (others => '0');
	s_out_u1 <= (others => '0');
	s_out_u2 <= (others => '0');
      else
	s_datastate_n1 <= s_datastate;
	case s_datastate_n1 is
--	case s_datastate is
	  when st_data_u2 =>
	  -- save sum of the butterfly
	    s_out_y1 <= s_result1_shift;
	    s_out_y2 <= s_result2_shift;
	  when st_data_y2 =>
	  -- save substraction of the butterfly
	    s_out_u1 <= s_result1_shift;
	    s_out_u2 <= s_result2_shift;
	  when others =>
	end case;
      end if;
    end if;
  end process p_butterfly_reg;
  p_butterfly : process (clk)
  begin -- process p_butterfly
    if rising_edge(clk) then  -- rising clock edge
      case s_datastate is
      -- perform complex multiplication (step 1)
	when st_data_y1 =>
	  s_dsp_bus.mul_in_a1 <= s_data_u1;
	  s_dsp_bus.mul_in_a2 <= s_data_u2;
	  s_dsp_bus.mul_in_b1 <= std_logic_vector(s_omega1);
	  s_dsp_bus.mul_in_b2 <= std_logic_vector(s_omega2);
	  s_dsp_bus.alu_select <= alu_cmul;
	  s_dsp_bus.acc_mode1 <= acc_store;
	  s_dsp_bus.acc_mode2 <= acc_store;
      -- perform complex multiplication (step 2)
	when st_data_y2 =>
	  s_dsp_bus.mul_in_a1 <= s_data_u1;
	  s_dsp_bus.mul_in_a2 <= s_data_u2;
	  s_dsp_bus.mul_in_b1 <= std_logic_vector(s_omega1);
	  s_dsp_bus.mul_in_b2 <= std_logic_vector(s_omega2);
	  s_dsp_bus.alu_select <= alu_cmul;
	  s_dsp_bus.acc_mode1 <= acc_store;
	  s_dsp_bus.acc_mode2 <= acc_store;
      -- sum of the butterfly
	when st_data_u1 =>
	  s_dsp_bus.mul_in_a1 <= s_data_y1;
	  s_dsp_bus.mul_in_a2 <= s_data_y2;
--	s_dsp_bus.mul_in_b1 <= std_logic_vector(to_signed(1, sig_width));
 --	s_dsp_bus.mul_in_b2 <= std_logic_vector(to_signed(1, sig_width));
	  s_dsp_bus.mul_in_b1 <= sig_cst_init(-0.99999);
	  s_dsp_bus.mul_in_b2 <= sig_cst_init(-0.99999);
	  s_dsp_bus.alu_select <= alu_mul;
	  s_dsp_bus.acc_mode1 <= acc_sub;
	  s_dsp_bus.acc_mode2 <= acc_sub;
      -- substraction of the butterfly
	when st_data_u2 =>
	  s_dsp_bus.mul_in_a1 <= s_data_y1;
	  s_dsp_bus.mul_in_a2 <= s_data_y2;
	  s_dsp_bus.mul_in_b1 <= sig_cst_init(-0.99999);
	  s_dsp_bus.mul_in_b2 <= sig_cst_init(-0.99999);
	  s_dsp_bus.alu_select <= alu_mul;
	  s_dsp_bus.acc_mode1 <= acc_minback_sub;
	  s_dsp_bus.acc_mode2 <= acc_minback_sub;
	when others =>
	  s_dsp_bus.mul_in_a1 <= (others => '0');
	  s_dsp_bus.mul_in_a2 <= (others => '0');
	  s_dsp_bus.mul_in_b1 <= (others => '0');
	  s_dsp_bus.mul_in_b2 <= (others => '0');
	  s_dsp_bus.alu_select <= alu_mul;
	  s_dsp_bus.acc_mode1 <= acc_store;
	  s_dsp_bus.acc_mode2 <= acc_store;
      end case;
    end if;
  end process p_butterfly;
  -------------------------------------------------------------------------------
  -- Compute address of reading words according to Cooley-Tukey
  -------------------------------------------------------------------------------
  p_addr_comput : process (clk)
  begin -- process p_addr_comput
    if rising_edge(clk) then  -- rising clock edge
      if s_state = st_init then
	s_radix_count <= to_unsigned(2, c_ind_width);
	s_butter_index <= to_unsigned(0, c_ind_width);
	s_butter_group <= to_unsigned(0, c_ind_width);
	s_butter_offset <= to_unsigned(0, c_ind_width);
	s_imag_part <= '0';
	s_end_ft <= '0';

	-- s_radix_count_down <= '0' & s_length((c_ind_width - 1) downto 1);
	s_radix_count_down <= to_unsigned(2**(angle_width - 1), angle_width);
	s_angle <= to_unsigned(0, angle_width);
	s_angle_offset <= to_unsigned(0, angle_width);
	-- init shift ctrl
	s_shift_flags_reg <= shift_flags_reg;
      else
	-- the real datastate is shifted of 3 stages because of pipeline delay
	if (s_datastate = st_data_y2) then
	  -- y1 being read, compute index of y2
	  s_butter_offset <= to_unsigned(0, c_ind_width);
	  s_imag_part <= '1';
	  -- sinus for ifft (no angle offset) or -sin for fft (+ pi)
	  if(opflag_select(opflagbit_ifft) = '1') then
	    s_angle_offset <= to_unsigned(0, angle_width);
	  else
	    s_angle_offset <= to_unsigned((2**(angle_width - 1)), angle_width);
	  end if;

	  -- s_angle_offset <= to_unsigned((2**(angle_width - 2)), angle_width);
	elsif (s_datastate = st_data_u1) then
	  -- y2 being read, compute index of u1
	  s_butter_offset <= s_radix_half;
	  s_imag_part <= '0';
	  -- angle + pi/2 to get cosinus
	  s_angle_offset <= to_unsigned((2**(angle_width - 2)), angle_width);
	  -- s_angle_offset <= to_unsigned(0, angle_width);
	elsif (s_datastate = st_data_u2) then
	  -- u1 being read, compute index of u2
	  s_butter_offset <= s_radix_half;
	  s_imag_part <= '1';

	-- else compute index of next sample
	elsif (s_next_index < s_radix_half) then
	  -- increment index
	  s_butter_index <= s_next_index((c_ind_width - 1) downto 0);
	  s_butter_offset <= to_unsigned(0, c_ind_width);
	  s_imag_part <= '0';
	  -- increment angle
	  s_angle <= s_angle + s_radix_count_down;
	elsif (s_next_group < s_length_moins) then
	  -- next group
	  s_butter_index <= to_unsigned(0, c_ind_width);
	  s_butter_group <= s_next_group((c_ind_width - 1) downto 0);
	  s_butter_offset <= to_unsigned(0, c_ind_width);
	  s_imag_part <= '0';
	  -- reset angle
	  s_angle <= to_unsigned(0, angle_width);
	elsif(s_radix_count = s_length) then
	  s_end_ft <= '1';
	else
	  -- next radix (left shift)
	  s_radix_count <= s_radix_count((c_ind_width - 2) downto 0) & '0';
	  s_radix_count_down <= '0' & s_radix_count_down((angle_width - 1) downto 1);
	  s_butter_group <= to_unsigned(0, c_ind_width);
	  s_butter_index <= to_unsigned(0, c_ind_width);
	  s_butter_offset <= to_unsigned(0, c_ind_width);
	  s_imag_part <= '0';
	  -- reset angle
	  s_angle <= to_unsigned(0, angle_width);
	  -- shift control (progressive division of the signal avoiding overflow)
	  s_shift_flags_reg <= '0' & s_shift_flags_reg((cmdreg_width - 1) downto 1);
        end if;
      end if;
      -- Insert one pipe stage to trigo to sync with data from memory m0
      s_angle_total <= std_logic_vector(s_angle + s_angle_offset);
    end if;
  end process p_addr_comput;
  s_next_index <= s_butter_index + 1;
  s_next_group <= s_butter_group + s_radix_count;
  -------------------------------------------------------------------------------
  -- address pipe : output is writting address
  -------------------------------------------------------------------------------
  p_addr_pipe : process (clk)
  begin -- process p_addr_pipe
    if rising_edge(clk) then  -- rising clock edge
      s_addr_pipe(0) <= s_addr_r_m0_tmp; -- s_dsp_bus.addr_r_m0;
      if(s_state = st_performft) then
	  s_wr_pipe(0) <= not s_end_ft;
      else
	  s_wr_pipe(0) <= '0';
      end if;
      for i in 0 to c_addr_pipe_depth - 2 loop
	s_addr_pipe(i + 1) <= s_addr_pipe(i);
	s_wr_pipe(i + 1) <= s_wr_pipe(i);
      end loop;
    end if;
  end process p_addr_pipe;

  p_shift_pipe : process (clk)
  begin -- process p_shift_pipe
    if rising_edge(clk) then  -- rising clock edge
      s_shift_pipe(0) <= s_shift_flags_reg(0);
      for i in 0 to c_shiftflag_pipe_depth - 2 loop
	s_shift_pipe(i + 1) <= s_shift_pipe(i);
      end loop;
    end if;
  end process p_shift_pipe;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  dsp_bus           <= s_dsp_bus;
  s_dsp_bus.data_out_m2         <= (others => '0');
  s_dsp_bus.data_out_m1         <= data_in_m2;
  s_dsp_bus.c_en_m0         <= '1';
  s_dsp_bus.c_en_m1         <= '1';
  s_dsp_bus.c_en_m2         <= '1';
  s_dsp_bus.gcounter_reset  <= '1';
  -- Writing and reading address of the memory
  s_sample_index <= s_butter_index + s_butter_group + s_butter_offset;
  s_sample_index_rev <= bit_reverse(s_sample_index);

  s_addr_r_m0_tmp((cmdreg_width - 1) downto (c_ind_width + 1)) <= (others => '0');

  -- index with bit reverse if needed
  s_addr_r_m0_tmp((c_ind_width) downto 1) <= s_sample_index when opflag_select(opflagbit_bitrev)='0' else
			 zeros(c_ind_width - 4) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 4))
			    when s_length(4) = '1' else
			 zeros(c_ind_width - 5) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 5))
			    when s_length(5) = '1' else
			 zeros(c_ind_width - 6) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 6))
			    when s_length(6) = '1' else
			 zeros(c_ind_width - 7) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 7))
			    when s_length(7) = '1' else
			 zeros(c_ind_width - 8) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 8))
			    when s_length(8) = '1' else
			 zeros(c_ind_width - 9) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 9))
			    when s_length(9) = '1' else
			 zeros(c_ind_width - 10) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 10))
			    when s_length(10) = '1' else
			 zeros(c_ind_width - 11) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 11))
			    when s_length(11) = '1' else
			 zeros(c_ind_width - 12) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 12))
			    when s_length(12) = '1' else
			 zeros(c_ind_width - 13) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 13))
			    when s_length(13) = '1' else
			 s_sample_index_rev;


  s_addr_r_m0_tmp(0) <= s_imag_part;

  p_addr_delay : process (clk)
  begin -- process p_shift_pipe
    if rising_edge(clk) then  -- rising clock edge
      s_dsp_bus.addr_r_m0 <= s_addr_r_m0_tmp;
    end if;
  end process;

  s_dsp_bus.addr_w_m0 <= s_addr_pipe(c_addr_pipe_depth - 1);
  s_dsp_bus.wr_en_m0 <= s_wr_pipe(c_addr_pipe_depth - 1);


  -- specific index relations
  s_length          <= unsigned(length_reg);
  s_length_moins	    <= s_length - 1;
  s_radix_half	    <= '0' & s_radix_count((c_ind_width - 1) downto 1);

  -- trigonometry
  s_dsp_bus.lut_in((angle_width - 1) downto 0) <= s_angle_total;
  s_dsp_bus.lut_in((lut_in_width - 1) downto angle_width) <= (others => '0');
  s_dsp_bus.lut_select <= lutsel_cos;
  -- shift control
  s_result1_shift <= result1((sig_width - 1) downto 0) when s_shift_pipe(c_shiftflag_pipe_depth - 1) = '0'
		     else result1(sig_width downto 1);
  s_result2_shift <= result2((sig_width - 1) downto 0) when s_shift_pipe(c_shiftflag_pipe_depth - 1) = '0'
		     else result2(sig_width downto 1);
 end archi_fft;

