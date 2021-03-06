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
-------------------------------------------------------------------------------

entity bench_dspunit is
end bench_dspunit;
--=----------------------------------------------------------------------------
architecture archi_bench_dspunit of bench_dspunit is
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
      clk             : in  std_logic;
      clk_cpu         : in  std_logic;
      reset           : in  std_logic;
      data_in_m0      : in  std_logic_vector((sig_width - 1) downto 0);
      data_out_m0     : out std_logic_vector((sig_width - 1) downto 0);
      addr_r_m0       : out std_logic_vector((cmdreg_width - 1) downto 0);
      addr_w_m0       : out std_logic_vector((cmdreg_width - 1) downto 0);
      wr_en_m0        : out std_logic;
      c_en_m0         : out std_logic;
      data_in_m1      : in  std_logic_vector((sig_width - 1) downto 0);
      data_out_m1     : out std_logic_vector((sig_width - 1) downto 0);
      addr_m1         : out std_logic_vector((cmdreg_width - 1) downto 0);
      wr_en_m1        : out std_logic;
      c_en_m1         : out std_logic;
      data_in_m2      : in  std_logic_vector((sig_width - 1) downto 0);
      data_out_m2     : out std_logic_vector((sig_width - 1) downto 0);
      addr_m2         : out std_logic_vector((cmdreg_width - 1) downto 0);
      wr_en_m2        : out std_logic;
      c_en_m2         : out std_logic;
      addr_cmdreg     : in  std_logic_vector((cmdreg_addr_width - 1) downto 0);
      data_in_cmdreg  : in  std_logic_vector((cmdreg_data_width - 1) downto 0);
      wr_en_cmdreg    : in  std_logic;
      data_out_cmdreg : out std_logic_vector((cmdreg_data_width - 1) downto 0);
      debug           : out std_logic_vector(15 downto 0);
      irq             : out std_logic;
      op_done         : out std_logic
      );
  end component;
  component gen_memoryf
    generic (
      addr_width : natural;
      data_width : natural;
      init_file  : string
      );
    port (
      address_a : in  std_logic_vector((addr_width - 1) downto 0);
      address_b : in  std_logic_vector((addr_width - 1) downto 0);
      clock_a   : in  std_logic;
      clock_b   : in  std_logic;
      data_a    : in  std_logic_vector((data_width - 1) downto 0);
      data_b    : in  std_logic_vector((data_width - 1) downto 0);
      wren_a    : in  std_logic;
      wren_b    : in  std_logic;
      q_a       : out std_logic_vector((data_width - 1) downto 0);
      q_b       : out std_logic_vector((data_width - 1) downto 0)
      );
  end component;
  component gen_memory
    generic (
      addr_width : natural;
      data_width : natural
      );
    port (
      address_a : in  std_logic_vector((addr_width - 1) downto 0);
      address_b : in  std_logic_vector((addr_width - 1) downto 0);
      clock_a   : in  std_logic;
      clock_b   : in  std_logic;
      data_a    : in  std_logic_vector((data_width - 1) downto 0);
      data_b    : in  std_logic_vector((data_width - 1) downto 0);
      wren_a    : in  std_logic;
      wren_b    : in  std_logic;
      q_a       : out std_logic_vector((data_width - 1) downto 0);
      q_b       : out std_logic_vector((data_width - 1) downto 0)
      );
  end component;
  component clock_gen
    generic (
      tpw : time;
      tps : time
      );
    port (
      clk   : out std_logic;
      reset : out std_logic
      );
  end component;
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_clk             : std_logic;
  signal s_reset           : std_logic;
  signal s_data_in_m0      : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_out_m0     : std_logic_vector((sig_width - 1) downto 0);
  signal s_addr_r_m0       : std_logic_vector((cmdreg_width - 1) downto 0);
  signal s_addr_w_m0       : std_logic_vector((cmdreg_width - 1) downto 0);
  signal s_wr_en_m0        : std_logic;
  signal s_c_en_m0         : std_logic;
  signal s_data_in_m1      : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_out_m1     : std_logic_vector((sig_width - 1) downto 0);
  signal s_addr_m1         : std_logic_vector((cmdreg_width - 1) downto 0);
  signal s_wr_en_m1        : std_logic;
  signal s_c_en_m1         : std_logic;
  signal s_data_in_m2      : std_logic_vector((sig_width - 1) downto 0);
  signal s_data_out_m2     : std_logic_vector((sig_width - 1) downto 0);
  signal s_addr_m2         : std_logic_vector((cmdreg_width - 1) downto 0);
  signal s_wr_en_m2        : std_logic;
  signal s_c_en_m2         : std_logic;
  signal s_addr_cmdreg     : std_logic_vector((cmdreg_addr_width - 1) downto 0);
  signal s_data_in_cmdreg  : std_logic_vector((cmdreg_data_width - 1) downto 0);
  signal s_wr_en_cmdreg    : std_logic;
  signal s_data_out_cmdreg : std_logic_vector((cmdreg_data_width - 1) downto 0);
  signal s_op_done         : std_logic;
  signal s_debug_dsp       : std_logic_vector(15 downto 0);
  signal s_irq             : std_logic;
begin  -- archs_bench_dspunit
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  dspunit_1 : dspunit
    port map (
      clk             => s_clk,
      clk_cpu         => s_clk,
      reset           => s_reset,
      data_in_m0      => s_data_in_m0,
      data_out_m0     => s_data_out_m0,
      addr_r_m0       => s_addr_r_m0,
      addr_w_m0       => s_addr_w_m0,
      wr_en_m0        => s_wr_en_m0,
      c_en_m0         => s_c_en_m0,
      data_in_m1      => s_data_in_m1,
      data_out_m1     => s_data_out_m1,
      addr_m1         => s_addr_m1,
      wr_en_m1        => s_wr_en_m1,
      c_en_m1         => s_c_en_m1,
      data_in_m2      => s_data_in_m2,
      data_out_m2     => s_data_out_m2,
      addr_m2         => s_addr_m2,
      wr_en_m2        => s_wr_en_m2,
      c_en_m2         => s_c_en_m2,
      addr_cmdreg     => s_addr_cmdreg,
      data_in_cmdreg  => s_data_in_cmdreg,
      wr_en_cmdreg    => s_wr_en_cmdreg,
      data_out_cmdreg => s_data_out_cmdreg,
      debug           => s_debug_dsp,
      irq             => s_irq,
      op_done         => s_op_done);

  gen_memory_1 : gen_memoryf
    generic map (
      addr_width => 16,
      data_width => 16,
--        init_file     => "exsig.mif")
      init_file  => "exsig_fft.mif")
--        init_file     => "Ones.mif")
    port map (
      address_a => s_addr_r_m0,
      address_b => s_addr_w_m0,
      clock_a   => s_clk,
      clock_b   => s_clk,
      data_a    => (others => '0'),
      data_b    => s_data_out_m0,
      wren_a    => '0',
      wren_b    => s_wr_en_m0,
      q_a       => s_data_in_m0,
      q_b       => open);

  gen_memory_2 : gen_memoryf
    generic map (
      addr_width => 16,
      data_width => 16,
      init_file  => "exsig_fft.mif")
    port map (
      address_a => s_addr_m1,
      address_b => (others => '0'),
      clock_a   => s_clk,
      clock_b   => s_clk,
      data_a    => s_data_out_m1,
      data_b    => (others => '0'),
      wren_a    => s_wr_en_m1,
      wren_b    => '0',
      q_a       => s_data_in_m1,
      q_b       => open);

  gen_memory_3 : gen_memory
    generic map (
      addr_width => 16,
      data_width => 16)
    port map (
      address_a => s_addr_m2,
      address_b => (others => '0'),
      clock_a   => s_clk,
      clock_b   => s_clk,
      data_a    => s_data_out_m2,
      data_b    => (others => '0'),
      wren_a    => s_wr_en_m2,
      wren_b    => '0',
      q_a       => s_data_in_m2,
      q_b       => open);

  clock_gen_1 : clock_gen
    generic map (
      tpw => 5 ns,
      tps => 0 ns)
    port map (
      clk   => s_clk,
      reset => s_reset);

  --=---------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  s_addr_cmdreg <= "0000", "0100" after 141 ns, "0010" after 151 ns, "0111" after 161 ns, "1000" after 171 ns,
--                         "0010" after 8751 ns, "0111" after 8761 ns, "1000" after 8771 ns,
                   "0100" after 241 ns, "0010" after 251 ns, "0111" after 261 ns, "1000" after 271 ns,
                   "0001" after 321 ns,
                   "0010" after 341 ns, "0100" after 351 ns, "0111" after 361 ns, "1000" after 371 ns,
                   "0100" after 461 ns, "0010" after 471 ns, "0111" after 481 ns, "1000" after 491 ns,
                   "0010" after 541 ns, "0100" after 551 ns, "0111" after 561 ns, "1000" after 571 ns,
                   "0100" after 661 ns, "0010" after 671 ns, "0111" after 681 ns, "1000" after 691 ns;
  s_data_in_cmdreg <= x"0000", x"003F" after 141 ns, x"0040" after 151 ns, x"003C" after 161 ns, x"0002" after 171 ns,  -- ifft bitrev
--                         x"003F" after 8751 ns, x"002D" after 8761 ns, x"0002" after 8771 ns,     -- dotcmul bitrev
                      -- sigshift is not available any more
                      -- x"0072" after 241 ns, x"0080" after 251 ns, x"0026" after 261 ns, x"0002" after 271 ns,           -- sigshift bitrev
                      x"0080" after 321 ns,
                      x"0040" after 341 ns, x"000F" after 351 ns, x"000C" after 361 ns, x"0002" after 371 ns,  -- fft
                      x"0040" after 461 ns, x"0040" after 471 ns, x"000D" after 481 ns, x"000A" after 491 ns,  -- dotcmul
                      x"0040" after 541 ns, x"000A" after 551 ns, x"003C" after 561 ns, x"0002" after 571 ns,  -- ifft bitrev
                      x"0040" after 661 ns, x"0040" after 671 ns, x"002D" after 681 ns, x"0002" after 691 ns;  -- dotcmul bitrev
  s_wr_en_cmdreg <= '0', '1' after 141 ns, '0' after 181 ns,
                    '1' after 241 ns, '0' after 281 ns,
                    '1' after 321 ns, '0' after 331 ns,
                    '1' after 341 ns, '0' after 381 ns,
                    '1' after 461 ns, '0' after 501 ns,
                    '1' after 541 ns, '0' after 581 ns,
                    '1' after 661 ns, '0' after 701 ns;
end archi_bench_dspunit;
-------------------------------------------------------------------------------
--   Simulation parameters
-->SIMSTOPTIME=35000ns
-->SIMSAVFILE=debugfft.sav
-------------------------------------------------------------------------------
