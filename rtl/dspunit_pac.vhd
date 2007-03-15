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


use std.textio.all;
library ieee;
use ieee.std_logic_1164.all;
--use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.dspalu_pac.all;
-------------------------------------------------------------------------------

package dspunit_pac is

constant sig_width	    : positive := 16;
constant cmdreg_addr_width  : natural := 6;
constant cmdreg_data_width  : positive := 16;
constant cmdreg_width	    : positive := 16;
constant acc_width          : positive := 40;


function sig_cst_init(realval : real) return std_logic_vector;
function module(a : signed; b : signed) return integer;
procedure dispsig(name : string; ind : integer; val : integer);

--type t_dsp_cmdregs is array (0 to ((2**cmdreg_addr_width) - 1)) of std_logic_vector((cmdreg_width - 1) downto 0);
type t_dsp_cmdregs is array (0 to 15) of std_logic_vector((cmdreg_width - 1) downto 0);
type t_dsp_bus is
  record
    op_done		    : std_logic;
    -- memory 0
    data_out_m0              : std_logic_vector((sig_width - 1) downto 0);
    addr_r_m0                : unsigned((cmdreg_width - 1) downto 0);
    addr_w_m0                : unsigned((cmdreg_width - 1) downto 0);
    wr_en_m0                 : std_logic;
    c_en_m0                  : std_logic;
    -- memory 1
    data_out_m1              : std_logic_vector((sig_width - 1) downto 0);
    addr_m1                : unsigned((cmdreg_width - 1) downto 0);
    wr_en_m1                 : std_logic;
    c_en_m1                  : std_logic;
    -- memory 2
    data_out_m2              : std_logic_vector((sig_width - 1) downto 0);
    addr_m2                : unsigned((cmdreg_width - 1) downto 0);
    wr_en_m2                 : std_logic;
    c_en_m2                  : std_logic;
    -- alu
    mul_in_a1		    : std_logic_vector((sig_width - 1) downto 0);
    mul_in_b1		    : std_logic_vector((sig_width - 1) downto 0);
    mul_in_a2		    : std_logic_vector((sig_width - 1) downto 0);
    mul_in_b2		    : std_logic_vector((sig_width - 1) downto 0);
    acc_mode1               : t_acc_mode;
    acc_mode2               : t_acc_mode;
    alu_select              : t_alu_select;
    cmp_mode                : t_cmp_mode;
    cmp_pol                 : std_logic;
    cmp_store               : std_logic;
    -- global counter
    gcounter_reset          : std_logic;
  end record;

constant c_dsp_bus_init            : t_dsp_bus := (
  op_done		   => '0',
  -- memory 0
  data_out_m0          => (others => '0'),
  addr_r_m0            => (others => '0'),
  addr_w_m0            => (others => '0'),
  wr_en_m0             => '0',
  c_en_m0              => '0',
  -- memory 1
  data_out_m1          => (others => '0'),
  addr_m1              => (others => '0'),
  wr_en_m1             => '0',
  c_en_m1              => '0',
  -- memory 2
  data_out_m2          => (others => '0'),
  addr_m2              => (others => '0'),
  wr_en_m2             => '0',
  c_en_m2              => '0',
  -- alu
  mul_in_a1		   => (others => '0'),
  mul_in_b1		   => (others => '0'),
  mul_in_a2		   => (others => '0'),
  mul_in_b2		   => (others => '0'),
  acc_mode1            => acc_store,
  acc_mode2            => acc_store,
  alu_select           => alu_mul,
  cmp_mode             => cmp_acc1,
  cmp_pol              => '0',
  cmp_store            => '1',
  -- global counter
  gcounter_reset       => '1'
);
-------------------------------------------------------------------------------
-- Register address
-------------------------------------------------------------------------------

-- registers offsets of dspunit
constant DSPADDR_STARTADDR0  : positive := 1;
constant DSPADDR_LENGTH0  : positive := 2;
constant DSPADDR_STARTADDR1  : positive := 3;
constant DSPADDR_LENGTH1  : positive := 4;
constant DSPADDR_STARTADDR2  : positive := 5;
constant DSPADDR_LENGTH2  : positive := 6;
constant DSPADDR_OPCODE : positive := 7;
constant DSPADDR_SR : positive := 8;

-- Bits of status register
constant DSP_SRBIT_OPDONE : natural := 0;
constant DSP_SRBIT_RUN : natural := 1;
constant DSP_SRBIT_LOADED : natural := 2;

-- opcodes of availables processings
constant opcode_width : positive := 4;
constant opcode_conv_circ : std_logic_vector((opcode_width - 1) downto 0) := "0001";
constant opcode_cpflip : std_logic_vector((opcode_width - 1) downto 0) := "0010";
constant opcode_cpmem : std_logic_vector((opcode_width - 1) downto 0) := "0100";
constant opcode_setmem : std_logic_vector((opcode_width - 1) downto 0) := "0101";
constant opcode_sigshift : std_logic_vector((opcode_width - 1) downto 0) := "0110";

-- opflags (options related to each operation)
constant opflag_width : positive := 8;
constant opflag_bitrev : std_logic_vector((opflag_width - 1) downto 0) := "00000010";
constant opflagbit_bitrev : natural := 1;
constant opflag_mainmem : std_logic_vector((opflag_width - 1) downto 0) := "00000010";
constant opflagbit_mainmem : natural := 1;
constant opflag_savestep : std_logic_vector((opflag_width - 1) downto 0) := "00001000";
constant opflagbit_savestep : natural := 3;

constant opflag_m0 : std_logic_vector((opflag_width - 1) downto 0) := "00100000";
constant opflagbit_m0 : natural := 5;
constant opflag_m1 : std_logic_vector((opflag_width - 1) downto 0) := "01000000";
constant opflagbit_m1 : natural := 6;
constant opflag_m2 : std_logic_vector((opflag_width - 1) downto 0) := "10000000";
constant opflagbit_m2 : natural := 7;


end dspunit_pac;

package body dspunit_pac is

function sig_cst_init(realval : real) return std_logic_vector
is
  variable fracval : real;
  variable fracint : integer;
  begin
    fracval := realval * real(2 ** (sig_width - 1));
    fracint := integer(floor(fracval));
    return std_logic_vector(to_signed(fracint, sig_width));
  end sig_cst_init;

function module(a : signed; b : signed) return integer
is
  variable res : real;
begin
  res := sqrt(real(to_integer(a))**2 + real(to_integer(b))**2);
  return integer(res);
end module;


procedure dispsig(name : string; ind : integer; val : integer)
is
  variable msg : line;
begin
  write(msg, String'("dispsig : "));
  write(msg, name);
  write(msg, String'("("));
  write(msg, ind);
  write(msg, String'(") = "));
  write(msg, val);

  report msg.all;
end dispsig;

end dspunit_pac;

