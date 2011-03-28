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


entity dsp_cmdpipe is
  port (
    reset   : in  std_logic;
    clk     : in  std_logic;
    -- data in port
    cmd_out : out t_dsp_cmdregs;
    read    : in  std_logic;
    empty   : out std_logic;
    -- data out port
    cmd_in  : in  t_dsp_cmdregs;
    write   : in  std_logic;
    full    : out std_logic
    );
end dsp_cmdpipe;
--=----------------------------------------------------------------------------
architecture archi_dsp_cmdpipe of dsp_cmdpipe is
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  type   t_dsp_cmdpipe is array(0 to (c_dsp_pipe_length - 1)) of t_dsp_cmdregs;
  signal s_loaded : std_logic_vector((c_dsp_pipe_length - 1) downto 0);
  signal s_unload : std_logic_vector((c_dsp_pipe_length - 1) downto 0);
  signal s_pipe   : t_dsp_cmdpipe;
begin  -- archs_dsp_cmdpipe
  p_pipe : process (clk, reset)
  begin  -- process p_pipe
    if reset = '0' then                 -- asynchronous reset
      s_loaded <= (others => '0');
    elsif rising_edge(clk) then         -- rising clock edge
      -- loading first tap
      if write = '1' then
        s_loaded(0) <= '1';
        s_pipe(0)   <= cmd_in;
      elsif s_unload(0) = '1' then
        s_loaded(0) <= '0';
      end if;
      -- pipe
      for i in 1 to c_dsp_pipe_length - 1 loop
        if s_loaded(i) = '0' or s_unload(i) = '1' then
          s_pipe(i)       <= s_pipe(i - 1);
          s_loaded(i)     <= s_loaded(i - 1);
          s_unload(i - 1) <= '1';
        else
          s_unload(i - 1) <= '0';
        end if;
      end loop;
      -- unloading last tap
      s_unload(c_dsp_pipe_length - 1) <= read;
    end if;
  end process p_pipe;
  full    <= s_loaded(0);
  empty   <= not s_loaded(c_dsp_pipe_length - 1);
  cmd_out <= s_pipe(c_dsp_pipe_length - 1);
end archi_dsp_cmdpipe;
-------------------------------------------------------------------------------
