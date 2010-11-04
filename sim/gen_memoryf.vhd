--   ----------------------------------------------------------------------
--   DspUnit : Advanced So(P)C Sequential Signal Processor
--   Copyright (C) 2006-2010 by Adrien LELONG (www.lelongdunet.com)
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


use std.textio.all;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
USE IEEE.STD_LOGIC_TEXTIO.ALL ;
-------------------------------------------------------------------------------

entity gen_memoryf is
  generic (
    addr_width : natural := 11;
    data_width : natural := 8;
    init_file : STRING
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
end gen_memoryf;
--=----------------------------------------------------------------------------
architecture archi_gen_memoryf of gen_memoryf is
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
    file data : TEXT IS init_file;
    variable lineStr, msg : line;
    variable i : natural := 0;
    variable rom_buf : std_logic_vector((data_width - 1) downto 0);
    variable ok : boolean;
    begin
      for i in 0 to (2**addr_width - 1) loop
	if not endfile(data) then
	  readline(data, lineStr);
	  hread(lineStr, rom_buf, ok);
	  if ok then
	    result(i) := rom_buf;
--	    result(i)((data_width - 2) downto 0) := rom_buf((data_width - 1) downto 1);
--	    result(i)(data_width - 1) := rom_buf(data_width - 1);
	  else
	    write(msg, String'(" !!! FORMAT !!! : "));
	--	write(msg, String'(line.all));
	    write(msg, String'(" @ line : "));
	    write(msg, i);
	    report msg.all;
	  end if;
	else
	  result(i) := (others => '0');
      end if;
      end loop;
      return result;
    end initialize_ram;
  signal s_ram_block         : memType := initialize_ram;
  signal s_address_a     : std_logic_vector((addr_width - 1) downto 0);
  signal s_address_b     : std_logic_vector((addr_width - 1) downto 0);
  signal s_w_clk         : std_logic;
begin  -- archs_gen_memoryf
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

--  -- Read initial content of the ram from file
--  file_read : process
--    file data : TEXT IS init_file;
--    variable lineStr, msg : line;
--    variable i : natural := 0;
--    variable rom_buf : std_logic_vector((data_width - 1) downto 0);
--    variable ok : boolean;
--  begin -- process file_read
--    while not endfile(data) loop
--      readline(data, lineStr);
--      hread(lineStr, rom_buf, ok);
--      if ok then
--	s_ram_block(i) <= rom_buf;	-- no conversion
--      else
--	write(msg, String'(" !!! FORMAT !!! : "));
----	write(msg, String'(line.all));
--	write(msg, String'(" @ line : "));
--	write(msg, i);
--	report msg.all;
--      end if;
--
--      if i < (2**addr_width - 1) then
--	i := i + 1;
--      else
--	report "ram full";
--	exit;
--      end if;
--    end loop;
--    wait;
--  end process file_read;
--

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
end archi_gen_memoryf;
-------------------------------------------------------------------------------
