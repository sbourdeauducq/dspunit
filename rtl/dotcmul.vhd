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
entity dotcmul is
  port (
    --@inputs
	 clk                      : in std_logic;
	 op_en                    : in std_logic;
	 data_in_m0               : in std_logic_vector((sig_width - 1) downto 0);
	 data_in_m1               : in std_logic_vector((sig_width - 1) downto 0);
	 length_reg               : in std_logic_vector((cmdreg_data_width -1) downto 0);
	 length_kern_reg          : in std_logic_vector((cmdreg_data_width -1) downto 0);
	 opflag_select            : in std_logic_vector((opflag_width - 1) downto 0);
	 result1                    : in std_logic_vector((sig_width - 1) downto 0);
	 result2                    : in std_logic_vector((sig_width - 1) downto 0);
    --@outputs;
	 dsp_bus                  : out t_dsp_bus
       );
end dotcmul;
--=----------------------------------------------------------------------------
architecture archi_dotcmul of dotcmul is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
    constant c_addr_pipe_depth         : integer := 9;
    constant c_ind_width               : integer := cmdreg_width - 2;
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_dsp_bus           : t_dsp_bus;
  type t_dotcmul_state is (st_init, st_startpipe, st_performft, st_copy);
  type t_datastate is (st_data_y1, st_data_y2);
  signal s_state             : t_dotcmul_state;
  signal s_length            : unsigned((cmdreg_width - 1) downto 0);
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
  type t_addr_pipe is array(0 to c_addr_pipe_depth - 1) of unsigned((cmdreg_width - 1) downto 0);
  type t_wr_pipe is array(0 to c_addr_pipe_depth - 1) of std_logic;
  signal s_addr_pipe         : t_addr_pipe;
  signal s_wr_pipe           : t_wr_pipe;
  signal s_next_index        : unsigned((c_ind_width - 1) downto 0);
  signal s_next_group        : unsigned((c_ind_width - 1) downto 0);
  signal s_sample_index      : unsigned((c_ind_width - 1) downto 0);
  signal s_sample_index_rev      : unsigned((c_ind_width - 1) downto 0);
  signal s_addr_r_m0_tmp     : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_r_m1_tmp     : unsigned((cmdreg_width - 1) downto 0);
  signal s_imag_part         : std_logic;
--  signal s_module            : integer;
  signal s_mask_reg          : unsigned((cmdreg_width - 1) downto 0);
  signal s_length_kern       : unsigned((cmdreg_width - 1) downto 0);
begin  -- archs_dotcmul
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  p_dotcmul : process (clk)
  begin -- process p_dotcmul
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
	  when st_performft =>
	    -- In this state : reading, computing butterfly and writting
	      -- are done concurently
	    -- if s_radix_half > s_length then
	    if s_sample_index = s_length then
	      --s_dsp_bus.wr_en_m1 <= '1';
	      s_state <= st_copy;
	    end if;
	  when st_copy =>
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
  end process p_dotcmul;
  -------------------------------------------------------------------------------
  -- Data states
  -------------------------------------------------------------------------------
  p_data : process (clk)
  begin -- process p_data
    if rising_edge(clk) then  -- rising clock edge
      if s_state = st_init then
	  -- initial state is calculated as a function of pipeline depth
	  s_datastate <= st_data_y1;
	  s_dsp_bus.alu_select <= alu_mul;
	  s_dsp_bus.acc_mode1 <= acc_store;
	  s_dsp_bus.acc_mode2 <= acc_store;
      else
	  case s_datastate is
	      when st_data_y1 =>
		  s_datastate <= st_data_y2;
	      when others =>	-- st_data_y2
		  s_datastate <= st_data_y1;
		  s_dsp_bus.alu_select <= alu_cmul_conj;
		  s_dsp_bus.acc_mode1 <= acc_store;
		  s_dsp_bus.acc_mode2 <= acc_store;
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
	  s_data_u1_r <= data_in_m1;
	  s_data_y1_r <= data_in_m0;
        when others => -- st_data_y2
	  dispsig("sigcmul", to_integer(s_sample_index) + 1, to_integer(signed(s_data_y1_r)));
	  s_data_y1 <= s_data_y1_r;
	  s_data_y2 <= data_in_m0;
	  s_data_u2 <= data_in_m1;
	  s_data_u1 <= s_data_u1_r;
      end case;
    end if;
  end process p_dataload;
  -------------------------------------------------------------------------------
  -- store data to memory (previous butterfly)
  -------------------------------------------------------------------------------
  p_datastore : process (clk)
  begin -- process p_datastore
    if rising_edge(clk) then  -- rising clock edge
      case s_datastate is
        when st_data_y1 =>
	  -- states y1, y2 inverted for writing because pipe length is odd
	  s_dsp_bus.data_out_m0 <= s_out_y1;
	  s_out_y2_r <= s_out_y2;
        when others =>  -- st_data_y1
	  s_dsp_bus.data_out_m0 <= s_out_y2_r;

	  s_out_y1 <= result1;
	  s_out_y2 <= result2;
      end case;
    end if;
  end process p_datastore;
  -------------------------------------------------------------------------------
  -- Compute address of reading words according to Cooley-Tukey
  -------------------------------------------------------------------------------
  p_addr_comput : process (clk)
  begin -- process p_addr_comput
    if rising_edge(clk) then  -- rising clock edge
      if s_state = st_init then
	s_sample_index <= to_unsigned(0, c_ind_width);
	s_imag_part <= '0';
      else
	-- the real datastate is shifted of 2 stages because of pipeline delay
	if (s_datastate = st_data_y1) then
	  -- y1 being read, compute index of y2
	  s_imag_part <= '1';

	-- else compute index of next sample
	-- elsif (s_next_index < s_length) then
	else
	  -- increment index
	  s_sample_index <= s_next_index((c_ind_width - 1) downto 0);
	  s_imag_part <= '0';
	end if;
      end if;
    end if;
  end process p_addr_comput;
  s_next_index <= s_sample_index + 1;
  -------------------------------------------------------------------------------
  -- address pipe : output is writting address
  -------------------------------------------------------------------------------
  p_addr_pipe : process (clk)
  begin -- process p_addr_pipe
    if rising_edge(clk) then  -- rising clock edge
      s_addr_pipe(0) <= s_dsp_bus.addr_r_m0;
      if(s_state = st_performft) then
	  s_wr_pipe(0) <= '1';
      else
	  s_wr_pipe(0) <= '0';
      end if;
      for i in 0 to c_addr_pipe_depth - 2 loop
	s_addr_pipe(i + 1) <= s_addr_pipe(i);
	s_wr_pipe(i + 1) <= s_wr_pipe(i);
      end loop;
    end if;
  end process p_addr_pipe;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  dsp_bus           <= s_dsp_bus;
  s_dsp_bus.data_out_m2         <= (others => '0');
  s_dsp_bus.data_out_m1         <= (others => '0');
  s_dsp_bus.c_en_m0         <= '1';
  s_dsp_bus.c_en_m1         <= '1';
  s_dsp_bus.c_en_m2         <= '1';
  s_dsp_bus.gcounter_reset  <= '1';
  -- alu inputs
  s_dsp_bus.mul_in_a1 <= s_data_y1;
  s_dsp_bus.mul_in_a2 <= s_data_y2;
  s_dsp_bus.mul_in_b1 <= s_data_u1;
  s_dsp_bus.mul_in_b2 <= s_data_u2;
  -- Writing and reading address of the memory
  s_sample_index_rev <= bit_reverse(s_sample_index);

  s_addr_r_m0_tmp((cmdreg_width - 1) downto (c_ind_width + 1)) <= (others => '0');
  s_addr_r_m1_tmp((cmdreg_width - 1) downto (c_ind_width + 1)) <= (others => '0');

  s_addr_r_m1_tmp((c_ind_width) downto 1) <= s_sample_index;
  -- index with bit reverse if needed
  s_addr_r_m0_tmp((c_ind_width) downto 1) <= s_sample_index when opflag_select(opflagbit_bitrev)='0' else
			 zeros(c_ind_width - 4) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 4))
			    when length_kern_reg(4) = '1' else
			 zeros(c_ind_width - 5) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 5))
			    when length_kern_reg(5) = '1' else
			 zeros(c_ind_width - 6) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 6))
			    when length_kern_reg(6) = '1' else
			 zeros(c_ind_width - 7) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 7))
			    when length_kern_reg(7) = '1' else
			 zeros(c_ind_width - 8) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 8))
			    when length_kern_reg(8) = '1' else
			 zeros(c_ind_width - 9) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 9))
			    when length_kern_reg(9) = '1' else
			 zeros(c_ind_width - 10) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 10))
			    when length_kern_reg(10) = '1' else
			 zeros(c_ind_width - 11) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 11))
			    when length_kern_reg(11) = '1' else
			 zeros(c_ind_width - 12) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 12))
			    when length_kern_reg(12) = '1' else
			 zeros(c_ind_width - 13) &
			    s_sample_index_rev((c_ind_width - 1) downto (c_ind_width - 13))
			    when length_kern_reg(13) = '1' else
			 s_sample_index_rev;


  s_addr_r_m0_tmp(0) <= s_imag_part;
  s_addr_r_m1_tmp(0) <= s_imag_part;

  s_dsp_bus.addr_r_m0 <= s_addr_r_m0_tmp;

--  s_dsp_bus.addr_m1 <= s_dsp_bus.addr_r_m0;
--  s_dsp_bus.addr_m1 <= s_addr_r_m1_tmp and (not s_mask_reg);
  s_dsp_bus.addr_m1 <= unsigned(bitbit_and(std_logic_vector(s_addr_r_m1_tmp),
		       std_logic_vector(s_mask_reg)));
  s_dsp_bus.addr_w_m0 <= s_addr_pipe(c_addr_pipe_depth - 1);
  s_dsp_bus.wr_en_m0 <= s_wr_pipe(c_addr_pipe_depth - 1);


  -- specific index relations
  s_length          <= unsigned(length_reg);
  -- left shift because real length is double (complex values)
  s_length_kern     <= unsigned(length_kern_reg((cmdreg_data_width - 2) downto 0) & '0');
  s_mask_reg         <= s_length_kern - 1;

--  s_module <= module(signed(s_data_y1), signed(s_data_y2));
 end archi_dotcmul;

