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
use work.dspalu_pac.all;
-------------------------------------------------------------------------------

entity bench_dspalu is
end bench_dspalu;
--=----------------------------------------------------------------------------
architecture archi_bench_dspalu of bench_dspalu is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
    constant sig_width               : integer := 16;
    constant acc_width               : integer := 40;
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
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
  component dspalu_acc
    generic (
      sig_width : integer ;
      acc_width : integer
	);
    port (
      a1          : in  std_logic_vector((sig_width - 1) downto 0);
      b1          : in  std_logic_vector((sig_width - 1) downto 0);
      a2          : in  std_logic_vector((sig_width - 1) downto 0);
      b2          : in  std_logic_vector((sig_width - 1) downto 0);
      clk         : in  std_logic;
      clr_acc     : in  std_logic;
      acc_mode1                : in t_acc_mode;
      acc_mode2                : in t_acc_mode;
      alu_select               : in t_alu_select;
      cmp_mode    : in  t_cmp_mode;
      cmp_pol     : in  std_logic;
      cmp_store   : in  std_logic;
      chain_acc   : in  std_logic;
      result1     : out std_logic_vector((sig_width - 1) downto 0);
      result_acc1 : out std_logic_vector((acc_width - 1) downto 0);
      result2     : out std_logic_vector((sig_width - 1) downto 0);
      result_acc2 : out std_logic_vector((acc_width - 1) downto 0);
      result_sum  : out std_logic_vector((2*sig_width - 1) downto 0);
      cmp_reg     : out std_logic_vector((acc_width - 1) downto 0);
      cmp_greater : out std_logic;
      cmp_out     : out std_logic
	);
  end component;
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_reset             : std_logic;
  signal s_a1                : std_logic_vector((sig_width - 1) downto 0);
  signal s_b1                : std_logic_vector((sig_width - 1) downto 0);
  signal s_a2                : std_logic_vector((sig_width - 1) downto 0);
  signal s_b2                : std_logic_vector((sig_width - 1) downto 0);
  signal s_clk               : std_logic;
  signal s_clr_acc           : std_logic;
  signal s_acc_mode1         : std_logic_vector((acc_mode_width - 1) downto 0); -- t_acc_mode;
  signal s_acc_mode2         : std_logic_vector((acc_mode_width - 1) downto 0); -- t_acc_mode;
  signal s_alu_select        : std_logic_vector((alu_select_width - 1) downto 0); -- t_alu_select;
  signal s_result1           : std_logic_vector((sig_width - 1) downto 0);
  signal s_result_acc1       : std_logic_vector((acc_width - 1) downto 0);
  signal s_result2           : std_logic_vector((sig_width - 1) downto 0);
  signal s_result_acc2       : std_logic_vector((acc_width - 1) downto 0);
  signal s_cmp_mode          : t_cmp_mode;
  signal s_cmp_pol           : std_logic;
  signal s_cmp_store         : std_logic;
  signal s_chain_acc         : std_logic;
  signal s_cmp_reg           : std_logic_vector((acc_width - 1) downto 0);
  signal s_cmp_greater       : std_logic;
  signal s_cmp_out           : std_logic;
  signal s_result_sum        : std_logic_vector((2*sig_width - 1) downto 0);
begin  -- archs_bench_dspalu
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  clock_gen_1 : clock_gen
    generic map (
	  tpw 	=> 5 ns,
	  tps 	=> 0 ns)
    port map (
	  clk 	=> s_clk,
	  reset 	=> s_reset);

  dspalu_acc_1 : dspalu_acc
    generic map (
	  sig_width 	=> sig_width,
	  acc_width 	=> acc_width)
    port map (
	  a1 	=> s_a1,
	  b1 	=> s_b1,
	  a2 	=> s_a2,
	  b2 	=> s_b2,
	  clk 	=> s_clk,
	  clr_acc 	=> s_clr_acc,
	  acc_mode1 	=> s_acc_mode1,
	  acc_mode2 	=> s_acc_mode2,
	  alu_select 	=> s_alu_select,
	  cmp_mode 	=> s_cmp_mode,
	  cmp_pol 	=> s_cmp_pol,
	  cmp_store 	=> s_cmp_store,
	  chain_acc 	=> s_chain_acc,
	  result1 	=> s_result1,
	  result_acc1 	=> s_result_acc1,
	  result2 	=> s_result2,
	  result_acc2 	=> s_result_acc2,
	  result_sum 	=> s_result_sum,
	  cmp_reg 	=> s_cmp_reg,
	  cmp_greater 	=> s_cmp_greater,
	  cmp_out 	=> s_cmp_out);

  --=---------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
   s_a1               <= "0010000100000000", "0000000000000010" after 501 ns;
   s_b1               <= "1111111111111011", "0000000000000011" after 501 ns;
   s_a2               <= "0000000000000100";
   s_b2               <= "1111111111111110";
   s_clr_acc         <= not s_reset;
   s_acc_mode1       <= acc_add, acc_store after 201 ns, acc_sub after 301 ns, acc_sumstore after 401 ns,
			acc_store after 501 ns, acc_sub after 701 ns, acc_add after 901 ns;
   s_acc_mode2       <= acc_add, acc_store after 501 ns, acc_sub after 701 ns, acc_add after 901 ns;
   s_alu_select      <= alu_mul, alu_cmul after 501 ns;
 end archi_bench_dspalu;
-------------------------------------------------------------------------------
