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
-- Simulation parameters
-->SIMSTOPTIME=3000ns
-->SIMSAVFILE=dspdiv.sav
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-------------------------------------------------------------------------------

entity bench_div is
end bench_div;
--=----------------------------------------------------------------------------
architecture archi_bench_div of bench_div is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
  constant c_sig_width                 : integer := 16;
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
  component dspdiv
    generic (
      sig_width : integer
	);
    port (
      num         : in  std_logic_vector((2*sig_width - 1) downto 0);
      den         : in  std_logic_vector((sig_width - 1) downto 0);
      clk         : in  std_logic;
    q                          : out std_logic_vector((sig_width - 1) downto 0);
    r                          : out std_logic_vector((2*sig_width - 3) downto 0)
	);
  end component;
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_clk               : std_logic;
  signal s_reset             : std_logic;
  signal s_num               : std_logic_vector((2*c_sig_width - 1) downto 0);
  signal s_den               : std_logic_vector((c_sig_width - 1) downto 0);
  signal s_q                 : std_logic_vector((c_sig_width - 1) downto 0);
  signal s_r                 : std_logic_vector((2*c_sig_width - 3) downto 0);
begin  -- archs_bench_div
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

  dspdiv_1 : dspdiv
    generic map (
	  sig_width 	=> c_sig_width)
    port map (
	  num 	=> s_num,
	  den 	=> s_den,
	  clk 	=> s_clk,
	  q 	=> s_q,
	  r 	=> s_r);

  --=---------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
    s_num <= x"00050000", x"05000000" after 21 ns, x"00050000" after 31 ns;
    s_den <= x"0406", x"0400" after 11 ns, x"FBFA" after 41 ns, x"0400" after 51 ns;
   -- s_num <= x"00050000",x"00050000" after 11 ns, x"FFFB0000" after 21 ns;
   -- s_den <= x"0406",x"FC00" after 31 ns;
 end archi_bench_div;
-------------------------------------------------------------------------------
