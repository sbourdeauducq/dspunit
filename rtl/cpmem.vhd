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
-------------------------------------------------------------------------------
entity cpmem is
  port (
    --@inputs
    clk        : in  std_logic;
    op_en      : in  std_logic;
    data_in_m0 : in  std_logic_vector((sig_width - 1) downto 0);
    length_reg : in  std_logic_vector((cmdreg_data_width -1) downto 0);
    --@outputs;
    dsp_bus    : out t_dsp_bus
    );
end cpmem;
--=----------------------------------------------------------------------------
architecture archi_cpmem of cpmem is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_dsp_bus : t_dsp_bus;
  type   t_cpmem_state is (st_init, st_startpipe, st_copy);
  signal s_state   : t_cpmem_state;
  signal s_length  : unsigned((cmdreg_width - 1) downto 0);
begin  -- archs_cpmem
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  p_cpmem : process (clk)
  begin  -- process p_cpmem
    if rising_edge(clk) then            -- rising clock edge
      if op_en = '0' then
        s_state              <= st_init;
        --s_dsp_bus <= c_dsp_bus_init;
        s_dsp_bus.op_done    <= '0';
        -- memory 0
        s_dsp_bus.addr_r_m0  <= (others => '0');
        s_dsp_bus.wr_en_m0   <= '0';
        -- memory 1
        s_dsp_bus.addr_m1    <= (others => '0');
        s_dsp_bus.wr_en_m1   <= '0';
        -------------------------------------------------------------------------------
        -- operation management
        -------------------------------------------------------------------------------
      else
        case s_state is
          when st_init =>
            s_dsp_bus.addr_m1   <= (others => '0');
            s_dsp_bus.addr_r_m0 <= (others => '0');
            s_dsp_bus.wr_en_m1  <= '0';
            if s_dsp_bus.op_done = '0' then
              s_state <= st_startpipe;
            end if;
          when st_startpipe =>
            if s_dsp_bus.addr_r_m0 = 1 then
              s_dsp_bus.wr_en_m1 <= '1';
              s_state            <= st_copy;
            end if;
            -- index increment
            s_dsp_bus.addr_r_m0 <= s_dsp_bus.addr_r_m0 + 1;
          when st_copy =>
            s_dsp_bus.wr_en_m1 <= '1';
            s_dsp_bus.wr_en_m0 <= '1';
            if(s_dsp_bus.addr_m1 = s_length) then
              s_state           <= st_init;
              s_dsp_bus.op_done <= '1';
            else
              s_dsp_bus.addr_r_m0 <= s_dsp_bus.addr_r_m0 + 1;
              s_dsp_bus.addr_m1   <= (s_dsp_bus.addr_m1 + 1) and s_length;
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process p_cpmem;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  dsp_bus                  <= s_dsp_bus;
  s_dsp_bus.addr_w_m0      <= s_dsp_bus.addr_m1;
  s_dsp_bus.data_out_m1    <= data_in_m0;
--  s_dsp_bus.data_out_m1         <= s_dsp_bus.addr_m1;
  s_dsp_bus.c_en_m0        <= op_en;
  s_dsp_bus.c_en_m1        <= op_en;
  s_length                 <= unsigned(length_reg);

  -- unused bus signals
  -- memory 0
  s_dsp_bus.data_out_m0    <= (others => '0');
  -- memory 2
  s_dsp_bus.data_out_m2    <= (others => '0');
  s_dsp_bus.addr_m2        <= (others => '0');
  s_dsp_bus.wr_en_m2       <= '0';
  s_dsp_bus.c_en_m2        <= '0';
  -- alu
  s_dsp_bus.mul_in_a1      <= (others => '0');
  s_dsp_bus.mul_in_b1      <= (others => '0');
  s_dsp_bus.mul_in_a2      <= (others => '0');
  s_dsp_bus.mul_in_b2      <= (others => '0');
  s_dsp_bus.acc_mode1      <= acc_none;
  s_dsp_bus.acc_mode2      <= acc_none;
  s_dsp_bus.alu_select     <= alu_none;
  s_dsp_bus.cmp_mode       <= cmp_none;
  s_dsp_bus.cmp_pol        <= '0';
  s_dsp_bus.cmp_store      <= '0';
  -- global counter
  s_dsp_bus.gcounter_reset <= '0';
  -- shared lut
  s_dsp_bus.lut_in         <= (others => '0');
  s_dsp_bus.lut_select     <= (others => '0');
end archi_cpmem;

