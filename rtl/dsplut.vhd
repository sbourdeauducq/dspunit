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

entity dsplut is
  port (
    --@inputs
    clk        : in  std_logic;
    lut_in     : in  std_logic_vector((lut_in_width - 1) downto 0);
    lut_select : in  std_logic_vector((lut_sel_width - 1) downto 0);
    --@outputs;
    lut_out    : out std_logic_vector((lut_out_width - 1) downto 0)
    );
end dsplut;
--=----------------------------------------------------------------------------
architecture archi_dsplut of dsplut is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  component gen_rom
    generic (
      addr_width : natural;
      data_width : natural;
      init_file  : string
      );
    port (
      address : in  std_logic_vector((addr_width - 1) downto 0);
      clk     : in  std_logic;
      q       : out std_logic_vector((data_width - 1) downto 0)
      );
  end component;
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_cos_out     : std_logic_vector((lut_out_width - 1) downto 0);
  signal s_cos_addr    : std_logic_vector((lut_in_width - 3) downto 0);
  signal s_cos_rom_out : std_logic_vector((lut_out_width - 1) downto 0);
  signal s_quart       : std_logic_vector(1 downto 0);
  signal s_quart_r1    : std_logic_vector(1 downto 0);
  signal s_quart_r2    : std_logic_vector(1 downto 0);
begin  -- archs_dsplut
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  cos_rom : gen_rom
    generic map (
      addr_width => lut_in_width - 2,
      data_width => lut_out_width,
      init_file  => "cos.mif")
    port map (
      address => s_cos_addr,
      clk     => clk,
      q       => s_cos_rom_out);

  --=---------------------------------------------------------------------------
  pipe : process (clk)
  begin  -- process pipe
    if rising_edge(clk) then            -- rising clock edge
      s_quart_r2 <= s_quart_r1;
      s_quart_r1 <= s_quart;
    end if;
  end process pipe;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  s_quart    <= lut_in((lut_in_width - 1) downto (lut_in_width - 2));
  s_cos_addr <= lut_in((lut_in_width - 3) downto 0) when s_quart(0) = '0' else
                not lut_in((lut_in_width - 3) downto 0);
  s_cos_out <= s_cos_rom_out when s_quart_r2(1) = '0' else
               not s_cos_rom_out;
  lut_out <= (others => '0') when lut_select = lutsel_none else
             s_cos_out when lut_select = lutsel_cos else
             s_cos_out when lut_select = lutsel_sin else
             (others => '0');
end archi_dsplut;
-------------------------------------------------------------------------------
