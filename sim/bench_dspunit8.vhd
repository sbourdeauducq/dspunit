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
-------------------------------------------------------------------------------

entity bench_dspunit8 is
end bench_dspunit8;
--=----------------------------------------------------------------------------
architecture archi_bench_dspunit of bench_dspunit8 is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  component dspunit
    port (
      clk                      : in std_logic;
      clk_cpu                      : in std_logic;
      reset                      : in std_logic;
      data_in_m0               : in std_logic_vector((sig_width - 1) downto 0);
      data_out_m0              : out std_logic_vector((sig_width - 1) downto 0);
      addr_r_m0                : out std_logic_vector((cmdreg_width - 1) downto 0);
      addr_w_m0                : out std_logic_vector((cmdreg_width - 1) downto 0);
      wr_en_m0                 : out std_logic;
      c_en_m0                  : out std_logic;
      data_in_m1               : in std_logic_vector((sig_width - 1) downto 0);
      data_out_m1              : out std_logic_vector((sig_width - 1) downto 0);
      addr_m1                : out std_logic_vector((cmdreg_width - 1) downto 0);
      wr_en_m1                 : out std_logic;
      c_en_m1                  : out std_logic;
      data_in_m2               : in std_logic_vector((sig_width - 1) downto 0);
      data_out_m2              : out std_logic_vector((sig_width - 1) downto 0);
      addr_m2                : out std_logic_vector((cmdreg_width - 1) downto 0);
      wr_en_m2                 : out std_logic;
      c_en_m2                  : out std_logic;
      addr_cmdreg               : in std_logic_vector((cmdreg_addr_width - 1) downto 0);
      data_in_cmdreg           : in std_logic_vector((cmdreg_data_width - 1) downto 0);
      wr_en_cmdreg                : in std_logic;
      data_out_cmdreg          : out std_logic_vector((cmdreg_data_width - 1) downto 0);
      debug                    : out std_logic_vector(15 downto 0);
      op_done                     : out std_logic
	);
  end component;
  component gen_memory
    generic (
      addr_width : natural ;
      data_width : natural
	);
    port (
      address_a                : in std_logic_vector((addr_width - 1) downto 0);
      address_b                : in std_logic_vector((addr_width - 1) downto 0);
      clock_a                    : in std_logic;
      clock_b                    : in std_logic;
      data_a                     : in std_logic_vector((data_width - 1) downto 0);
      data_b                     : in std_logic_vector((data_width - 1) downto 0);
      wren_a                     : in std_logic;
      wren_b                     : in std_logic;
      q_a                        : out std_logic_vector((data_width - 1) downto 0);
      q_b                        : out std_logic_vector((data_width - 1) downto 0)
	);
  end component;
  component clock_gen
    generic (
     tpw : time;
     tps : time
	);
    port (
      clk                      : out std_logic;
      reset                    : out std_logic
	);
  end component;
  component regBtoW
    generic (
      addr_width               : integer
	);
    port (
      reset                    : in std_logic;
      clk                      : in std_logic;
      data_in                  : in std_logic_vector(7 downto 0);
      addr_in                  : in std_logic_vector(addr_width downto 0);
      wr_in                    : in std_logic;
      regbank_sel              : in std_logic;
      data_out                 : out std_logic_vector(15 downto 0);
      addr_out                 : out std_logic_vector((addr_width - 1) downto 0);
      wr_out                   : out std_logic
	);
  end component;
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_clk               : std_logic;
  signal s_reset             : std_logic;
  signal s_data_in_m0        : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_out_m0       : std_logic_vector((sig_width - 1) downto 0);
  signal s_addr_r_m0         : std_logic_vector((cmdreg_width - 1) downto 0);
  signal s_addr_w_m0         : std_logic_vector((cmdreg_width - 1) downto 0);
  signal s_wr_en_m0          : std_logic;
  signal s_c_en_m0           : std_logic;
  signal s_data_in_m1        : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_out_m1       : std_logic_vector((sig_width - 1) downto 0);
  signal s_addr_m1           : std_logic_vector((cmdreg_width - 1) downto 0);
  signal s_wr_en_m1          : std_logic;
  signal s_c_en_m1           : std_logic;
  signal s_data_in_m2        : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_out_m2       : std_logic_vector((sig_width - 1) downto 0);
  signal s_addr_m2           : std_logic_vector((cmdreg_width - 1) downto 0);
  signal s_wr_en_m2          : std_logic;
  signal s_c_en_m2           : std_logic;
  signal s_addr_cmdreg       : std_logic_vector((cmdreg_addr_width - 1) downto 0);
  signal s_data_in_cmdreg    : std_logic_vector((cmdreg_data_width - 1) downto 0);
  signal s_wr_en_cmdreg      : std_logic;
  signal s_data_out_cmdreg   : std_logic_vector((cmdreg_data_width - 1) downto 0);
  signal s_op_done           : std_logic;
  signal s_data_in_cmdreg8   : std_logic_vector(7 downto 0);
  signal s_addr_cmdreg8      : std_logic_vector(cmdreg_addr_width downto 0);
  signal s_wr_en_cmdreg8     : std_logic;
  signal s_regbank_sel       : std_logic;
begin  -- archs_bench_dspunit
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  dspunit_1 : dspunit
    port map (
	  clk 	=> s_clk,
	  clk_cpu 	=> s_clk,
	  reset 	=> s_reset,
	  data_in_m0 	=> s_data_in_m0,
	  data_out_m0 	=> s_data_out_m0,
	  addr_r_m0 	=> s_addr_r_m0,
	  addr_w_m0 	=> s_addr_w_m0,
	  wr_en_m0 	=> s_wr_en_m0,
	  c_en_m0 	=> s_c_en_m0,
	  data_in_m1 	=> s_data_in_m1,
	  data_out_m1 	=> s_data_out_m1,
	  addr_m1 	=> s_addr_m1,
	  wr_en_m1 	=> s_wr_en_m1,
	  c_en_m1 	=> s_c_en_m1,
	  data_in_m2 	=> s_data_in_m2,
	  data_out_m2 	=> s_data_out_m2,
	  addr_m2 	=> s_addr_m2,
	  wr_en_m2 	=> s_wr_en_m2,
	  c_en_m2 	=> s_c_en_m2,
	  addr_cmdreg 	=> s_addr_cmdreg,
	  data_in_cmdreg 	=> s_data_in_cmdreg,
	  wr_en_cmdreg 	=> s_wr_en_cmdreg,
	  data_out_cmdreg 	=> s_data_out_cmdreg,
          debug => open,
	  op_done 	=> s_op_done);

  gen_memory_1 : gen_memory
    generic map (
	  addr_width 	=> 16,
	  data_width 	=> 16)
    port map (
	  address_a 	=> s_addr_r_m0,
	  address_b 	=> s_addr_w_m0,
	  clock_a 	=> s_clk,
	  clock_b 	=> s_clk,
	  data_a 	=> (others => '0'),
	  data_b 	=> s_data_out_m0,
	  wren_a 	=> '0',
	  wren_b 	=> s_wr_en_m0,
	  q_a 	=> s_data_in_m0,
	  q_b 	=> open);

  gen_memory_2 : gen_memory
    generic map (
	  addr_width 	=> 16,
	  data_width 	=> 16)
    port map (
	  address_a 	=> s_addr_m1,
	  address_b 	=> (others => '0'),
	  clock_a 	=> s_clk,
	  clock_b 	=> s_clk,
	  data_a 	=> s_data_out_m1,
	  data_b 	=> (others => '0'),
	  wren_a 	=> s_wr_en_m1,
	  wren_b 	=> '0',
	  q_a 	=> s_data_in_m1,
	  q_b 	=> open);

  gen_memory_3 : gen_memory
    generic map (
	  addr_width 	=> 16,
	  data_width 	=> 16)
    port map (
	  address_a 	=> s_addr_m2,
	  address_b 	=> (others => '0'),
	  clock_a 	=> s_clk,
	  clock_b 	=> s_clk,
	  data_a 	=> s_data_out_m2,
	  data_b 	=> (others => '0'),
	  wren_a 	=> s_wr_en_m2,
	  wren_b 	=> '0',
	  q_a 	=> s_data_in_m2,
	  q_b 	=> open);

  clock_gen_1 : clock_gen
    generic map (
	  tpw 	=> 2.5 ns,
	  tps 	=> 0 ns)
    port map (
	  clk 	=> s_clk,
	  reset 	=> s_reset);

  regBtoW_1 : regBtoW
    generic map (
	  addr_width 	=> cmdreg_addr_width)
    port map (
	  reset 	=> s_reset,
	  clk 	=> s_clk,
	  data_in 	=> s_data_in_cmdreg8,
	  addr_in 	=> s_addr_cmdreg8,
	  wr_in 	=> s_wr_en_cmdreg8,
	  regbank_sel 	=> s_regbank_sel,
	  data_out 	=> s_data_in_cmdreg,
	  addr_out 	=> s_addr_cmdreg,
	  wr_out 	=> s_wr_en_cmdreg);

  --=---------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
--  s_addr_cmdreg     <= "000000", "000010" after 151 ns, "000111" after 161 ns, "000000" after 171 ns,
--                         "000010" after 851 ns, "000111" after 861 ns, "000000" after 871 ns,
--                         "000010" after 3651 ns, "000111" after 3661 ns, "000000" after 3671 ns;
--  s_data_in_cmdreg  <= x"0000", x"000F" after 151 ns, x"0002" after 161 ns, x"0000" after 171 ns,
--                         x"000F" after 851 ns, x"0003" after 861 ns, x"0000" after 871 ns,
--                         x"000F" after 3651 ns, x"0002" after 3661 ns, x"0000" after 3671 ns;
--  s_wr_en_cmdreg    <= '0', '1' after 151 ns, '0' after 171 ns, '1' after 851 ns, '0' after 871 ns,
--                        '1' after 3651 ns, '0' after 3671 ns;
  s_addr_cmdreg8     <= "0000000", "0000111" after 141 ns, "0000110" after 146 ns, "0000101" after 151 ns, "0000100" after 156 ns, "0001111" after 161 ns, "0001110" after 166 ns, "0000001" after 171 ns, "0000000" after 176 ns;
--                         "0000101" after 851 ns, "0000100" after 856 ns, "0001111" after 861 ns, "0001110" after 861 ns, "000000" after 871 ns,
--                         "000010" after 3651 ns, "000111" after 3661 ns, "000000" after 3671 ns;
  s_data_in_cmdreg8  <= x"00", x"01" after 141 ns, x"32" after 146 ns, x"01" after 151 ns, x"FF" after 156 ns, x"00" after 161 ns, x"02" after 166 ns, x"00" after 171 ns, x"00" after 176 ns;
--                         x"000F" after 851 ns, x"0003" after 861 ns, x"0000" after 871 ns,
--                         x"000F" after 3651 ns, x"0002" after 3661 ns, x"0000" after 3671 ns;
  s_wr_en_cmdreg8    <= '0', '1' after 141 ns, '0' after 171 ns;-- '1' after 851 ns, '0' after 871 ns,
--                        '1' after 3651 ns, '0' after 3671 ns;
   s_regbank_sel     <= '1';
 end archi_bench_dspunit;
-------------------------------------------------------------------------------
--   Simulation parameters
-->SIMSTOPTIME=10000ns
-->SIMSAVFILE=
-------------------------------------------------------------------------------
