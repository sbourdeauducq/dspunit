--------------------------------------------------------------------------
--
--  Copyright (C) 1993, Peter J. Ashenden
--  Mail:	Dept. Computer Science
--		University of Adelaide, SA 5005, Australia
--  e-mail:	petera@cs.adelaide.edu.au
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 1, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--
--------------------------------------------------------------------------
--
--  $RCSfile: clock_gen.vhdl,v $  $Revision: 2.1 $  Date: 1993/10/31 20:20:50 $
--
--------------------------------------------------------------------------
--
--  Entity declaration for clock generator
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity clock_gen is
   generic (
   Tpw : Time;
   Tps : Time);
   port (
    --@inputs
    --@outputs;
    clk                      : out std_logic;
    reset                    : out std_logic
);
end clock_gen;
--=----------------------------------------------------------------------------
architecture archi_clock_gen of clock_gen is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
  constant clock_period : Time := 2*(Tpw+Tps);
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
begin  -- archs_clock_gen
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  p_gen_clk : process
  begin -- process p_gen_clk
    clk <= '1', '0' after Tpw;
    wait for clock_period;

  end process p_gen_clk;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  reset_driver:
    reset <= '0', '1' after 10*clock_period + Tpw+Tps;

 end archi_clock_gen;
-------------------------------------------------------------------------------
