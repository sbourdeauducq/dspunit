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
use work.dsputil_pac.all;
-------------------------------------------------------------------------------
entity dotdiv is
  port (
    --@inputs
    clk        : in  std_logic;
    op_en      : in  std_logic;
    data_in_m0 : in  std_logic_vector((sig_width - 1) downto 0);
    data_in_m1 : in  std_logic_vector((sig_width - 1) downto 0);
    data_in_m2 : in  std_logic_vector((sig_width - 1) downto 0);
    length_reg : in  std_logic_vector((cmdreg_data_width -1) downto 0);
    offset_result : in  std_logic_vector((cmdreg_data_width -1) downto 0);
    num_shift  : in std_logic_vector((cmdreg_data_width - 1) downto 0);
    opflag_select   : in  std_logic_vector((opflag_width - 1) downto 0);
    div_q      : in std_logic_vector((sig_width - 1) downto 0);
    --@outputs;
    dsp_bus    : out t_dsp_bus
    );
end dotdiv;
--=----------------------------------------------------------------------------
architecture archi_dotdiv of dotdiv is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
  constant c_dotdiv_pipe_length        : integer := div_pipe_length + 5;
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_dsp_bus : t_dsp_bus;
  type   t_dotdiv_state is (st_init, st_load_divpipe, st_store_divres);
  signal s_state   : t_dotdiv_state;
  signal s_length  : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_r            : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_w            : unsigned((cmdreg_width - 1) downto 0);
  signal s_addr_w_offs       : unsigned((cmdreg_width - 1) downto 0);
  signal s_wr_en             : std_logic;
  signal s_data_a            : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_b            : std_logic_vector((sig_width - 1) downto 0);
  signal s_div_num           : std_logic_vector((sig_width - 1) downto 0);
  signal s_num_shift         : std_logic_vector(2 downto 0);
  signal s_div_den_next      : std_logic_vector((sig_width - 1) downto 0);
  signal s_div_num_next      : std_logic_vector((2*sig_width - 1) downto 0);
  signal s_num_sign          : std_logic;
begin  -- archs_dotdiv
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  p_dotdiv : process (clk)
  begin  -- process p_dotdiv
    if rising_edge(clk) then            -- rising clock edge
      if op_en = '0' then
        s_state              <= st_init;
        s_dsp_bus.op_done    <= '0';
        s_addr_r  <= (others => '0');
        s_addr_w    <= (others => '0');
        s_wr_en   <= '0';
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
              s_state <= st_load_divpipe;
              s_addr_r <= (others => '0');
            end if;
          when st_load_divpipe =>
            s_addr_r <= s_addr_r + 1;
            if(s_addr_r = c_dotdiv_pipe_length) then
              s_wr_en <= '1';
              s_state <= st_store_divres;
            end if;
          when st_store_divres =>
            s_addr_r <= s_addr_r + 1;
            s_addr_w <= s_addr_w + 1;
            if s_addr_w = s_length then
            -- end of operation
              s_wr_en <= '0';
              s_state <= st_init;
              s_dsp_bus.op_done <= '1';
            end if;
          when others => null;
        end case;
      end if;
    end if;
  end process p_dotdiv;

  p_data_select : process (clk)
  begin -- process p_data_select
    if rising_edge(clk) then  -- rising clock edge
      case opflag_select(opflagbit_srcm2 downto opflagbit_srcm0) is
        when "011" =>
          s_data_a <= data_in_m0;
          s_data_b <= data_in_m1;
        when "101" =>
          s_data_a <= data_in_m0;
          s_data_b <= data_in_m2;
        when "110" =>
          s_data_a <= data_in_m1;
          s_data_b <= data_in_m2;
        when others =>
          s_data_a <= data_in_m0;
          s_data_b <= data_in_m1;
      end case;
      s_dsp_bus.div_num <= s_div_num_next;
      s_dsp_bus.div_den <= s_div_den_next;
      s_num_shift <= num_shift(2 downto 0);
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
      s_dsp_bus.data_out_m0 <= div_q;
      s_dsp_bus.data_out_m1 <= div_q;
      s_dsp_bus.data_out_m2 <= div_q;
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
        s_dsp_bus.addr_w_m0 <= s_addr_w_offs;
        s_dsp_bus.addr_r_m0 <= s_addr_r;
        if opflag_select(opflagbit_srcm1) = '1' then
          s_dsp_bus.addr_m1 <= s_addr_r;
        else
          s_dsp_bus.addr_m1 <= s_addr_w_offs;
        end if;
        if opflag_select(opflagbit_srcm2) = '1' then
          s_dsp_bus.addr_m2 <= s_addr_r;
        else
          s_dsp_bus.addr_m2 <= s_addr_w_offs;
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
  -- Divider input signals
  s_div_num <= s_data_a when opflag_select(opflagbit_srcswap) = '0' else s_data_b;
  s_div_den_next <= s_data_a when opflag_select(opflagbit_srcswap) = '1' else s_data_b;
  s_div_num_next <= s_div_num & zeros(sig_width) when s_num_shift = x"0" else
                       bit_extent(s_num_sign, 2) & s_div_num & zeros(sig_width - 2) when s_num_shift = x"1" else
                       bit_extent(s_num_sign, 4) & s_div_num & zeros(sig_width - 4) when s_num_shift = x"2" else
                       bit_extent(s_num_sign, 6) & s_div_num & zeros(sig_width - 6) when s_num_shift = x"3" else
                       bit_extent(s_num_sign, 8) & s_div_num & zeros(sig_width - 8) when s_num_shift = x"4" else
                       bit_extent(s_num_sign, 10) & s_div_num & zeros(sig_width - 10) when s_num_shift = x"5" else
                       bit_extent(s_num_sign, 12) & s_div_num & zeros(sig_width - 12) when s_num_shift = x"6" else
                       bit_extent(s_num_sign, 14) & s_div_num & zeros(sig_width - 14) when s_num_shift = x"7";
  s_num_sign <= s_div_num(sig_width - 1);


  dsp_bus                  <= s_dsp_bus;
  s_dsp_bus.gcounter_reset <= '1';
  s_length                 <= unsigned(length_reg);
  s_dsp_bus.alu_select <= alu_mul;
  s_addr_w_offs <= s_addr_w + unsigned(offset_result);
end archi_dotdiv;

