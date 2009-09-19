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
-------------------------------------------------------------------------------

entity regBtoW is
  generic (
    addr_width : integer
    );
  port (
    --@inputs
    reset       : in  std_logic;
    clk         : in  std_logic;
    data_in     : in  std_logic_vector(7 downto 0);
    addr_in     : in  std_logic_vector(addr_width downto 0);
    wr_in       : in  std_logic;
    regbank_sel : in  std_logic;
    --@outputs;
    data_out    : out std_logic_vector(15 downto 0);
    addr_out    : out std_logic_vector((addr_width - 1) downto 0);
    wr_out      : out std_logic
    );
end regBtoW;
--=----------------------------------------------------------------------------
architecture archi_BtoWreg of regBtoW is
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
  signal s_firstbyte_ok  : std_logic;
  signal s_addr_out      : std_logic_vector((addr_width - 1) downto 0);
  signal s_data_low_out  : std_logic_vector(7 downto 0);
  signal s_data_high_out : std_logic_vector(7 downto 0);
begin  -- archs_BtoWreg
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  p_conv : process (clk, reset)
  begin  -- process p_conv
    if reset = '0' then                 -- asynchronous reset
      s_addr_out      <= (others => '1');
      addr_out        <= (others => '1');
      s_data_low_out  <= (others => '0');
      s_data_high_out <= (others => '0');
      wr_out          <= '0';
    elsif rising_edge(clk) then         -- rising clock edge
      if (wr_in = '1' and regbank_sel = '1') then
        if s_firstbyte_ok = '1' then
          s_addr_out      <= addr_in(addr_width downto 1);
          addr_out        <= addr_in(addr_width downto 1);
          s_data_high_out <= data_in;
          wr_out          <= '0';
        elsif(s_addr_out = addr_in(addr_width downto 1)) then
          s_data_low_out <= data_in;
          addr_out       <= s_addr_out;
          s_addr_out     <= (others => '1');
          wr_out         <= '1';
        else
          wr_out     <= '0';
          s_addr_out <= (others => '1');
          addr_out   <= addr_in(addr_width downto 1);
        end if;
      else
        addr_out <= addr_in(addr_width downto 1);
        wr_out   <= '0';
      end if;
    end if;
  end process p_conv;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  s_firstbyte_ok <= addr_in(0);
  data_out       <= s_data_high_out & s_data_low_out;
end archi_BtoWreg;
-------------------------------------------------------------------------------
