--   ----------------------------------------------------------------------
--   DspUnit : Advanced So(P)C Sequential Signal Processor
--   Copyright (C) 2007-2010 by Adrien LELONG (www.lelongdunet.com)
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
    data_in_m1 : in  std_logic_vector((sig_width - 1) downto 0);
    data_in_m2 : in  std_logic_vector((sig_width - 1) downto 0);
    length_reg : in  std_logic_vector((cmdreg_data_width -1) downto 0);
    opflag_select   : in  std_logic_vector((opflag_width - 1) downto 0);
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
  signal s_addr_real_r            : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_real_w            : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_r            : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_w            : unsigned((cmdreg_width - 1) downto 0);
  signal s_wr_en             : std_logic;
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
        s_dsp_bus.op_done    <= '0';
        s_addr_r  <= (others => '0');
        s_addr_w    <= (others => '0');
        s_wr_en   <= '0';
        s_dsp_bus.acc_mode1  <= acc_store;
        s_dsp_bus.acc_mode2  <= acc_store;
        s_dsp_bus.alu_select <= alu_mul;
        -------------------------------------------------------------------------------
        -- operation management
        -------------------------------------------------------------------------------
      else
        case s_state is
          when st_init =>
            s_addr_w   <= (others => '0');
            s_addr_r <= (others => '0');
            s_wr_en  <= '0';
            if s_dsp_bus.op_done = '0' then
              s_state <= st_startpipe;
            end if;
          when st_startpipe =>
            if s_addr_r = 2 then
              s_wr_en <= '1';
              s_state            <= st_copy;
            end if;
            -- index increment
            s_addr_r <= s_addr_r + 1;
          when st_copy =>
            s_wr_en <= '1';
            if(s_addr_w = s_length) then
              s_state           <= st_init;
              s_dsp_bus.op_done <= '1';
            else
              s_addr_r <= s_addr_r + 1;
              s_addr_w   <= (s_addr_w + 1) and s_length;
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process p_cpmem;
  p_data_select : process (clk)
  begin -- process p_data_select
    if rising_edge(clk) then  -- rising clock edge
      if op_en = '0' then
        s_dsp_bus.data_out_m0 <= (others => '0');
        s_dsp_bus.data_out_m1 <= (others => '0');
        s_dsp_bus.data_out_m2 <= (others => '0');
      elsif opflag_select(opflagbit_srcm0) = '1' then
        s_dsp_bus.data_out_m0 <= data_in_m0;
        s_dsp_bus.data_out_m1 <= data_in_m0;
        s_dsp_bus.data_out_m2 <= data_in_m0;
      elsif opflag_select(opflagbit_srcm1) = '1' then
        s_dsp_bus.data_out_m0 <= data_in_m1;
        s_dsp_bus.data_out_m1 <= data_in_m1;
        s_dsp_bus.data_out_m2 <= data_in_m1;
      elsif opflag_select(opflagbit_srcm2) = '1' then
        s_dsp_bus.data_out_m0 <= data_in_m2;
        s_dsp_bus.data_out_m1 <= data_in_m2;
        s_dsp_bus.data_out_m2 <= data_in_m2;
      end if;
    end if;
  end process p_data_select;

  p_out_select : process (clk)
  begin -- process p_out_select
    if rising_edge(clk) then  -- rising clock edge
      if op_en = '0' then
        s_dsp_bus.wr_en_m0 <= '0';
        s_dsp_bus.wr_en_m1 <= '0';
        s_dsp_bus.wr_en_m2 <= '0';
      elsif opflag_select(opflagbit_m0) = '1' then
        s_dsp_bus.wr_en_m0 <= s_wr_en;
        s_dsp_bus.wr_en_m1 <= '0';
        s_dsp_bus.wr_en_m2 <= '0';
      elsif opflag_select(opflagbit_m1) = '1' then
        s_dsp_bus.wr_en_m0 <= '0';
        s_dsp_bus.wr_en_m1 <= s_wr_en;
        s_dsp_bus.wr_en_m2 <= '0';
      elsif opflag_select(opflagbit_m2) = '1' then
        s_dsp_bus.wr_en_m0 <= '0';
        s_dsp_bus.wr_en_m1 <= '0';
        s_dsp_bus.wr_en_m2 <= s_wr_en;
      end if;
    end if;
  end process p_out_select;

  p_adr_select : process (clk)
  begin -- process p_adr_select
    if rising_edge(clk) then  -- rising clock edge
      if op_en = '0' then
        s_dsp_bus.addr_r_m0 <= (others => '0');
        s_dsp_bus.addr_w_m0 <= (others => '0');
        s_dsp_bus.addr_m1 <= (others => '0');
        s_dsp_bus.addr_m2 <= (others => '0');
        s_dsp_bus.c_en_m0 <= '0';
        s_dsp_bus.c_en_m1 <= '0';
        s_dsp_bus.c_en_m2 <= '0';
      else
        s_dsp_bus.addr_w_m0 <= s_addr_real_w;
        s_dsp_bus.addr_r_m0 <= s_addr_real_r;
        if opflag_select(opflagbit_srcm1) = '1' then
          s_dsp_bus.addr_m1 <= s_addr_real_r;
        else
          s_dsp_bus.addr_m1 <= s_addr_real_w;
        end if;
        if opflag_select(opflagbit_srcm2) = '1' then
          s_dsp_bus.addr_m2 <= s_addr_real_r;
        else
          s_dsp_bus.addr_m2 <= s_addr_real_w;
        end if;
        s_dsp_bus.c_en_m0 <= opflag_select(opflagbit_srcm0) or opflag_select(opflagbit_m0);
        s_dsp_bus.c_en_m1 <= opflag_select(opflagbit_srcm1) or opflag_select(opflagbit_m1);
        s_dsp_bus.c_en_m2 <= opflag_select(opflagbit_srcm2) or opflag_select(opflagbit_m2);
      end if;
    end if;
  end process p_adr_select;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  dsp_bus                  <= s_dsp_bus;
  s_dsp_bus.gcounter_reset <= '1';
  s_length                 <= unsigned(length_reg);
  s_addr_real_w <= s_addr_w when opflag_select(opflagbit_tocomplex) = '0' else
                   s_addr_w((cmdreg_width - 2) downto 0) & '0';
  s_addr_real_r <= s_addr_r when opflag_select(opflagbit_fromcomplex) = '0' else
                   s_addr_r((cmdreg_width - 2) downto 0) & '0';
end archi_cpmem;

