--   ----------------------------------------------------------------------
--   DspUnit : Advanced So(P)C Sequential Signal Processor
--   Copyright (C) 2006-2009 by Adrien LELONG (www.lelongdunet.com)
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

entity gen_memory is
  generic (
    addr_width : natural := 11;
    data_width : natural := 8
  );
  port (
    --@inputs
    address_a                : in std_logic_vector((addr_width - 1) downto 0);
    address_b                : in std_logic_vector((addr_width - 1) downto 0);
    clock_a                    : in std_logic;
    clock_b                    : in std_logic;
    data_a                     : in std_logic_vector((data_width - 1) downto 0);
    data_b                     : in std_logic_vector((data_width - 1) downto 0);
    wren_a                     : in std_logic;
    wren_b                     : in std_logic;
    --@outputs;
    q_a                        : out std_logic_vector((data_width - 1) downto 0);
    q_b                        : out std_logic_vector((data_width - 1) downto 0)
);
end gen_memory;
--=----------------------------------------------------------------------------
architecture archi_gen_memory of gen_memory is
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
  type memType is array((2**addr_width - 1) downto 0) of std_logic_vector((data_width - 1) downto 0);
  -- Fonction d'initialisation du programme
  function initialize_ram
    return memType is
    variable result : memType;
    begin
      --result(3) := std_logic_vector(to_signed(50000, data_width));
--      result(16) := x"4000";
--      result(0) := x"7FFF";
--      result(11) := x"0000";
--      result(12) := x"4000";
--      for i in 0 to 15 loop
--        result(i) := (others => '0');
--      end loop;
--      for i in 17 to 29 loop
--        result(i) := (others => '0');
--      end loop;
      for i in 0 to (2**addr_width - 1) loop
        result(i) := (others => '0');
      end loop;
      return result;
    end initialize_ram;
  signal s_ram_block         : memType := initialize_ram;
  signal s_address_a     : std_logic_vector((addr_width - 1) downto 0);
  signal s_address_b     : std_logic_vector((addr_width - 1) downto 0);
  signal s_w_clk         : std_logic;
begin  -- archs_gen_memory
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  ramProc_a : process (clock_a)
  begin -- process ramProc
    if rising_edge(clock_a) then  -- rising clock edge
      s_address_a <= address_a;
      q_a <= s_ram_block(to_integer(unsigned(s_address_a)));
    end if;
  end process ramProc_a;

  ramProc_b : process (clock_b)
  begin -- process ramProc
    if rising_edge(clock_b) then  -- rising clock edge
      s_address_b <= address_b;
      q_b <= s_ram_block(to_integer(unsigned(s_address_b)));
    end if;
  end process ramProc_b;

  ramWrite : process (clock_a)
  begin -- process ramWrite
    -- if rising_edge(s_w_clk) then  -- rising clock edge
    if falling_edge(clock_a) then  -- rising clock edge
      if wren_a = '1' then
        s_ram_block(to_integer(unsigned(address_a))) <= data_a;
      elsif wren_b = '1' then
        s_ram_block(to_integer(unsigned(address_b))) <= data_b;
      end if;
    end if;
  end process ramWrite;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  s_w_clk              <= (clock_a and wren_a) or (clock_b and wren_b);
end archi_gen_memory;
-------------------------------------------------------------------------------
