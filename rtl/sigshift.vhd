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
entity sigshift is
  port (
    --@inputs
    clk           : in  std_logic;
    op_en         : in  std_logic;
    data_in_m0    : in  std_logic_vector((sig_width - 1) downto 0);
    length_reg    : in  std_logic_vector((cmdreg_width -1) downto 0);
    shift_reg     : in  std_logic_vector((cmdreg_width -1) downto 0);
    opflag_select : in  std_logic_vector((opflag_width - 1) downto 0);
    --@outputs;
    dsp_bus       : out t_dsp_bus
    );
end sigshift;
--=----------------------------------------------------------------------------
architecture archi_sigshift of sigshift is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
  constant c_addr_pipe_depth : integer := 3;
  constant c_data_pipe_depth : integer := 6;
  constant c_ind_width       : integer := cmdreg_width - 2;
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal   s_dsp_bus         : t_dsp_bus;
  type     t_sigshift_state is (st_init, st_cycle, st_cycleend);

  signal s_state              : t_sigshift_state;
  signal s_length             : unsigned((cmdreg_width - 1) downto 0);
  signal s_shift              : unsigned((cmdreg_width - 1) downto 0);
  signal s_length_moins       : unsigned((cmdreg_width - 1) downto 0);
  type   t_addr_pipe is array(0 to c_addr_pipe_depth - 1) of unsigned((cmdreg_width - 1) downto 0);
  type   t_data_pipe is array(0 to c_data_pipe_depth - 1) of std_logic_vector((sig_width - 1) downto 0);
  type   t_wr_pipe is array(0 to c_addr_pipe_depth - 1) of std_logic;
  signal s_addr_pipe          : t_addr_pipe;
  signal s_data_pipe          : t_data_pipe;
  signal s_data_bis           : std_logic_vector((sig_width - 1) downto 0);
  signal s_wr_pipe            : t_wr_pipe;
  signal s_next_index         : unsigned((c_ind_width - 1) downto 0);
  signal s_sample_index       : unsigned((c_ind_width - 1) downto 0);
  signal s_sample_index_rev   : unsigned((c_ind_width - 2) downto 0);
  signal s_sample_index_w     : unsigned((c_ind_width - 1) downto 0);
  signal s_sample_index_w_rev : unsigned((c_ind_width - 2) downto 0);
  signal s_addr_r_m0_tmp      : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_w_m0_tmp      : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_r_tmp         : unsigned((cmdreg_width - 1) downto 0);
begin  -- archs_sigshift
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  p_sigshift : process (clk)
  begin  -- process p_sigshift
    if rising_edge(clk) then            -- rising clock edge
      if op_en = '0' then
        s_state           <= st_init;
        --s_dsp_bus <= c_dsp_bus_init;
        s_dsp_bus.op_done <= '0';
        -------------------------------------------------------------------------------
        -- operation management
        -------------------------------------------------------------------------------
      else
        case s_state is
          when st_init =>
            if s_dsp_bus.op_done = '0' then
              s_state <= st_cycle;
            end if;
          when st_cycle =>
            if s_sample_index = s_length_moins + c_data_pipe_depth then
              s_state <= st_cycleend;
            end if;
          when st_cycleend =>
            if s_wr_pipe(c_addr_pipe_depth - 1) = '0' then
              -- cycle terminates when writing is about to stop
              s_state           <= st_init;
              s_dsp_bus.op_done <= '1';
            end if;
          when others =>
            s_state <= st_init;
        end case;
      end if;
    end if;
  end process p_sigshift;
  -------------------------------------------------------------------------------
  -- Compute address of reading words
  -------------------------------------------------------------------------------
  p_addr_comput : process (clk)
  begin  -- process p_addr_comput
    if rising_edge(clk) then            -- rising clock edge
      if(s_state = st_cycle) then
        s_sample_index <= s_next_index((c_ind_width - 1) downto 0);
      else
        s_sample_index <= to_unsigned(0, c_ind_width);
      end if;
    end if;
  end process p_addr_comput;
  s_next_index <= s_sample_index + 1;
  -------------------------------------------------------------------------------
  -- address pipe : output is writting address
  -------------------------------------------------------------------------------
  p_addr_pipe : process (clk)
  begin  -- process p_addr_pipe
    if rising_edge(clk) then            -- rising clock edge
      s_addr_pipe(0) <= s_addr_w_m0_tmp;
      if(s_state = st_cycle) then
        s_wr_pipe(0) <= '1';
      else
        s_wr_pipe(0) <= '0';
      end if;
      for i in 0 to c_addr_pipe_depth - 2 loop
        s_addr_pipe(i + 1) <= s_addr_pipe(i);
        s_wr_pipe(i + 1)   <= s_wr_pipe(i);
      end loop;
    end if;
  end process p_addr_pipe;
  s_addr_r_tmp <= zeros(cmdreg_width - c_ind_width) & s_sample_index;
  p_data_pipe : process (clk)
  begin  -- process p_data_pipe
    if rising_edge(clk) then            -- rising clock edge
      s_data_pipe(0) <= data_in_m0;
      for i in 0 to c_data_pipe_depth - 2 loop
        s_data_pipe(i + 1) <= s_data_pipe(i);
      end loop;
    end if;
  end process p_data_pipe;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  dsp_bus                  <= s_dsp_bus;
  s_dsp_bus.data_out_m2    <= (others => '0');
  s_dsp_bus.data_out_m1    <= (others => '0');
  s_dsp_bus.c_en_m0        <= '1';
  s_dsp_bus.c_en_m1        <= '1';
  s_dsp_bus.c_en_m2        <= '1';
  s_dsp_bus.gcounter_reset <= '1';

--  s_dsp_bus.data_out_m0 <= data_in_m0;
  s_data_bis <= s_data_pipe(c_data_pipe_depth - 1)(sig_width - 1) & s_data_pipe(c_data_pipe_depth - 1)((sig_width - 1) downto 1);
  -- Writing and reading address of the memory
  process (clk)
  begin  -- process
    if rising_edge(clk) then            -- rising clock edge
      -- One register just after address computation
      s_dsp_bus.addr_r_m0 <= s_addr_r_m0_tmp and s_length_moins;

      s_dsp_bus.data_out_m0 <= std_logic_vector(signed(data_in_m0) + signed(s_data_bis));
      s_dsp_bus.addr_w_m0   <= s_addr_pipe(c_addr_pipe_depth - 1) + s_length;
      s_dsp_bus.wr_en_m0    <= s_wr_pipe(c_addr_pipe_depth - 1);
    end if;
  end process;
--  s_dsp_bus.addr_w_m0 <= (s_addr_pipe(c_addr_pipe_depth - 1) + s_shift) and s_length_moins;
--  s_dsp_bus.addr_w_m0 <= s_addr_pipe(0) + s_length;

  -- Writing and reading address of the memory
  s_sample_index_rev   <= bit_reverse(s_sample_index((c_ind_width - 1) downto 1));
  s_sample_index_w     <= (s_sample_index + s_shift((c_ind_width - 1) downto 0)) and s_length_moins((c_ind_width - 1) downto 0);
  s_sample_index_w_rev <= bit_reverse(s_sample_index_w((c_ind_width - 1) downto 1));


  -- index with bit reverse if needed
  s_addr_r_m0_tmp((cmdreg_width - 1) downto c_ind_width) <= (others => '0');
  s_addr_r_m0_tmp((c_ind_width - 1) downto 1)            <= s_sample_index((c_ind_width - 1) downto 1) when opflag_select(opflagbit_bitrev) = '0' else
                                                            zeros(c_ind_width - 4) &
                                                            s_sample_index_rev((c_ind_width - 2) downto (c_ind_width - 4))
                                                            when s_length(4) = '1'  else
                                                            zeros(c_ind_width - 5) &
                                                            s_sample_index_rev((c_ind_width - 2) downto (c_ind_width - 5))
                                                            when s_length(5) = '1'  else
                                                            zeros(c_ind_width - 6) &
                                                            s_sample_index_rev((c_ind_width - 2) downto (c_ind_width - 6))
                                                            when s_length(6) = '1'  else
                                                            zeros(c_ind_width - 7) &
                                                            s_sample_index_rev((c_ind_width - 2) downto (c_ind_width - 7))
                                                            when s_length(7) = '1'  else
                                                            zeros(c_ind_width - 8) &
                                                            s_sample_index_rev((c_ind_width - 2) downto (c_ind_width - 8))
                                                            when s_length(8) = '1'  else
                                                            zeros(c_ind_width - 9) &
                                                            s_sample_index_rev((c_ind_width - 2) downto (c_ind_width - 9))
                                                            when s_length(9) = '1'  else
                                                            zeros(c_ind_width - 10) &
                                                            s_sample_index_rev((c_ind_width - 2) downto (c_ind_width - 10))
                                                            when s_length(10) = '1' else
                                                            zeros(c_ind_width - 11) &
                                                            s_sample_index_rev((c_ind_width - 2) downto (c_ind_width - 11))
                                                            when s_length(11) = '1' else
                                                            zeros(c_ind_width - 12) &
                                                            s_sample_index_rev((c_ind_width - 2) downto (c_ind_width - 12))
                                                            when s_length(12) = '1' else
                                                            zeros(c_ind_width - 13) &
                                                            s_sample_index_rev((c_ind_width - 2) downto (c_ind_width - 13))
                                                            when s_length(13) = '1' else
                                                            s_sample_index_rev;

  s_addr_r_m0_tmp(0) <= s_sample_index(0);

  -- index with bit reverse if needed
  s_addr_w_m0_tmp((cmdreg_width - 1) downto c_ind_width) <= (others => '0');
  s_addr_w_m0_tmp((c_ind_width - 1) downto 1)            <= s_sample_index_w((c_ind_width - 1) downto 1) when opflag_select(opflagbit_bitrev) = '0' else
                                                            zeros(c_ind_width - 4) &
                                                            s_sample_index_w_rev((c_ind_width - 2) downto (c_ind_width - 4))
                                                            when s_length(4) = '1'  else
                                                            zeros(c_ind_width - 5) &
                                                            s_sample_index_w_rev((c_ind_width - 2) downto (c_ind_width - 5))
                                                            when s_length(5) = '1'  else
                                                            zeros(c_ind_width - 6) &
                                                            s_sample_index_w_rev((c_ind_width - 2) downto (c_ind_width - 6))
                                                            when s_length(6) = '1'  else
                                                            zeros(c_ind_width - 7) &
                                                            s_sample_index_w_rev((c_ind_width - 2) downto (c_ind_width - 7))
                                                            when s_length(7) = '1'  else
                                                            zeros(c_ind_width - 8) &
                                                            s_sample_index_w_rev((c_ind_width - 2) downto (c_ind_width - 8))
                                                            when s_length(8) = '1'  else
                                                            zeros(c_ind_width - 9) &
                                                            s_sample_index_w_rev((c_ind_width - 2) downto (c_ind_width - 9))
                                                            when s_length(9) = '1'  else
                                                            zeros(c_ind_width - 10) &
                                                            s_sample_index_w_rev((c_ind_width - 2) downto (c_ind_width - 10))
                                                            when s_length(10) = '1' else
                                                            zeros(c_ind_width - 11) &
                                                            s_sample_index_w_rev((c_ind_width - 2) downto (c_ind_width - 11))
                                                            when s_length(11) = '1' else
                                                            zeros(c_ind_width - 12) &
                                                            s_sample_index_w_rev((c_ind_width - 2) downto (c_ind_width - 12))
                                                            when s_length(12) = '1' else
                                                            zeros(c_ind_width - 13) &
                                                            s_sample_index_w_rev((c_ind_width - 2) downto (c_ind_width - 13))
                                                            when s_length(13) = '1' else
                                                            s_sample_index_w_rev;

  s_addr_w_m0_tmp(0) <= s_sample_index_w(0);

  s_shift <= unsigned(shift_reg);

  s_dsp_bus.addr_m1  <= (others => '0');
  s_dsp_bus.wr_en_m1 <= '0';
  s_dsp_bus.wr_en_m2 <= '0';
  s_dsp_bus.addr_m2  <= to_unsigned(0, cmdreg_width);

  -- specific index relations
  s_length       <= unsigned(length_reg);
  s_length_moins <= s_length - 1;

end archi_sigshift;

