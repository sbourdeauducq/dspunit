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


use std.textio.all;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
USE IEEE.STD_LOGIC_TEXTIO.ALL ;
use work.rompkg.all;
-------------------------------------------------------------------------------

entity gen_rom is
  generic (
    addr_width : natural := 11;
    data_width : natural := 8;
    init_file : STRING
  );
  port (
    --@inputs
    address                  : in std_logic_vector((addr_width - 1) downto 0);
    clk                      : in std_logic;
    --@outputs;
    q                        : out std_logic_vector((data_width - 1) downto 0)
);
end gen_rom;
--=----------------------------------------------------------------------------
architecture archi_gen_rom of gen_rom is
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
  signal s_address             : std_logic_vector((addr_width - 1) downto 0);
  signal s_ram_block         : memType;
begin  -- archs_gen_rom
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  ramProc_a : process (clk)
  begin -- process ramProc
    if rising_edge(clk) then  -- rising clock edge
      s_address <= address;
      q <= s_ram_block(to_integer(unsigned(s_address)));
    end if;
  end process ramProc_a;

  file_read : process
    file data : TEXT IS init_file;
    variable lineStr, msg : line;
    variable i : natural := 0;
    variable rom_buf : std_logic_vector((data_width - 1) downto 0);
    variable ok : boolean;
  begin -- process file_read
    while not endfile(data) loop
      readline(data, lineStr);
      hread(lineStr, rom_buf, ok);
      if ok then
	s_ram_block(i) <= rom_buf;	-- no conversion
      else
	write(msg, String'(" !!! FORMAT !!! : "));
--	write(msg, String'(line.all));
	write(msg, String'(" @ line : "));
	write(msg, i);
	report msg.all;
      end if;

      if i < (2**addr_width - 1) then
	i := i + 1;
      else
	report "rom full";
	exit;
      end if;
    end loop;
    wait;
  end process file_read;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
 end archi_gen_rom;
-------------------------------------------------------------------------------
