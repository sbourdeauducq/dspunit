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
use work.bit_manipulation.all;
-------------------------------------------------------------------------------

package dspunit_pac is

  constant sig_width         : positive := 16;
  constant cmdreg_addr_width : natural  := 6;
  constant cmdreg_data_width : positive := 16;
  constant cmdreg_width      : positive := 16;
  constant acc_width         : positive := 40;
  constant acc_reduce_width  : positive := 30;
  constant lut_in_width      : positive := 13;
  constant lut_sel_width     : positive := 4;
  constant lut_out_width     : positive := sig_width;
  constant angle_width       : positive := 13;


  function sig_cst_init(realval : real) return std_logic_vector;
  function module(a             : signed; b : signed) return integer;
  procedure dispsig(name        : string; ind : integer; val : integer);

--type t_dsp_cmdregs is array (0 to ((2**cmdreg_addr_width) - 1)) of std_logic_vector((cmdreg_width - 1) downto 0);
  type t_dsp_cmdregs is array (0 to 15) of std_logic_vector((cmdreg_width - 1) downto 0);
  type t_dsp_bus is
  record
    op_done        : std_logic;
    -- memory 0
    data_out_m0    : std_logic_vector((sig_width - 1) downto 0);
    addr_r_m0      : unsigned((cmdreg_width - 1) downto 0);
    addr_w_m0      : unsigned((cmdreg_width - 1) downto 0);
    wr_en_m0       : std_logic;
    c_en_m0        : std_logic;
    -- memory 1
    data_out_m1    : std_logic_vector((sig_width - 1) downto 0);
    addr_m1        : unsigned((cmdreg_width - 1) downto 0);
    wr_en_m1       : std_logic;
    c_en_m1        : std_logic;
    -- memory 2
    data_out_m2    : std_logic_vector((sig_width - 1) downto 0);
    addr_m2        : unsigned((cmdreg_width - 1) downto 0);
    wr_en_m2       : std_logic;
    c_en_m2        : std_logic;
    -- alu
    mul_in_a1      : std_logic_vector((sig_width - 1) downto 0);
    mul_in_b1      : std_logic_vector((sig_width - 1) downto 0);
    mul_in_a2      : std_logic_vector((sig_width - 1) downto 0);
    mul_in_b2      : std_logic_vector((sig_width - 1) downto 0);
    acc_mode1      : std_logic_vector((acc_mode_width - 1) downto 0); -- t_acc_mode;
    acc_mode2      : std_logic_vector((acc_mode_width - 1) downto 0); -- t_acc_mode;
    alu_select     : std_logic_vector((alu_select_width - 1) downto 0); -- t_alu_select;
    cmp_mode       : std_logic_vector((cmp_mode_width - 1) downto 0); -- t_cmp_mode;
    cmp_pol        : std_logic;
    cmp_store      : std_logic;
    -- global counter
    gcounter_reset : std_logic;
    -- shared lut
    lut_in         : std_logic_vector((lut_in_width - 1) downto 0);
    lut_select     : std_logic_vector((lut_sel_width - 1) downto 0);
  end record;

  constant c_dsp_bus_init : t_dsp_bus := (
    op_done        => '0',
    -- memory 0
    data_out_m0    => (others => '0'),
    addr_r_m0      => (others => '0'),
    addr_w_m0      => (others => '0'),
    wr_en_m0       => '0',
    c_en_m0        => '0',
    -- memory 1
    data_out_m1    => (others => '0'),
    addr_m1        => (others => '0'),
    wr_en_m1       => '0',
    c_en_m1        => '0',
    -- memory 2
    data_out_m2    => (others => '0'),
    addr_m2        => (others => '0'),
    wr_en_m2       => '0',
    c_en_m2        => '0',
    -- alu
    mul_in_a1      => (others => '0'),
    mul_in_b1      => (others => '0'),
    mul_in_a2      => (others => '0'),
    mul_in_b2      => (others => '0'),
    acc_mode1      => acc_none,
    acc_mode2      => acc_none,
    alu_select     => alu_none,
    cmp_mode       => cmp_none,
    cmp_pol        => '0',
    cmp_store      => '0',
    -- global counter
    gcounter_reset => '0',
    -- shared lut
    lut_in         => (others => '0'),
    lut_select     => (others => '0')
    );

  function "or" (a, b : t_dsp_bus) return t_dsp_bus;
  function "and" (a : std_logic_vector; b : std_logic) return std_logic_vector;

-------------------------------------------------------------------------------
-- Register address
-------------------------------------------------------------------------------

-- registers offsets of dspunit
  constant DSPADDR_STARTADDR0 : positive := 1;
  constant DSPADDR_LENGTH0    : positive := 2;
  constant DSPADDR_STARTADDR1 : positive := 3;
  constant DSPADDR_LENGTH1    : positive := 4;
  constant DSPADDR_STARTADDR2 : positive := 5;
  constant DSPADDR_LENGTH2    : positive := 6;
  constant DSPADDR_OPCODE     : positive := 7;
  constant DSPADDR_SR         : positive := 8;

-- Bits of status register
  constant DSP_SRBIT_OPDONE : natural := 0;
  constant DSP_SRBIT_RUN    : natural := 1;
  constant DSP_SRBIT_LOADED : natural := 2;

-- opcodes of availables processings
  constant opcode_width     : positive                                      := 4;
  constant opcode_conv_circ : std_logic_vector((opcode_width - 1) downto 0) := "0001";
  constant opcode_cpflip    : std_logic_vector((opcode_width - 1) downto 0) := "0010";
  constant opcode_cpmem     : std_logic_vector((opcode_width - 1) downto 0) := "0100";
  constant opcode_setmem    : std_logic_vector((opcode_width - 1) downto 0) := "0101";
  constant opcode_sigshift  : std_logic_vector((opcode_width - 1) downto 0) := "0110";
  constant opcode_fft       : std_logic_vector((opcode_width - 1) downto 0) := "1100";
  constant opcode_dotcmul   : std_logic_vector((opcode_width - 1) downto 0) := "1101";

-- opflags (options related to each operation)
  constant opflag_width       : positive                                      := 8;
  constant opflag_ifft        : std_logic_vector((opflag_width - 1) downto 0) := "00000001";
  constant opflagbit_ifft     : natural                                       := 0;
  constant opflag_bitrev      : std_logic_vector((opflag_width - 1) downto 0) := "00000010";
  constant opflagbit_bitrev   : natural                                       := 1;
  constant opflag_mainmem     : std_logic_vector((opflag_width - 1) downto 0) := "00000010";
  constant opflagbit_mainmem  : natural                                       := 1;
  constant opflag_savestep    : std_logic_vector((opflag_width - 1) downto 0) := "00001000";
  constant opflagbit_savestep : natural                                       := 3;

  constant opflag_m0    : std_logic_vector((opflag_width - 1) downto 0) := "00100000";
  constant opflagbit_m0 : natural                                       := 5;
  constant opflag_m1    : std_logic_vector((opflag_width - 1) downto 0) := "01000000";
  constant opflagbit_m1 : natural                                       := 6;
  constant opflag_m2    : std_logic_vector((opflag_width - 1) downto 0) := "10000000";
  constant opflagbit_m2 : natural                                       := 7;

  constant opflag_srcm0    : std_logic_vector((opflag_width - 1) downto 0) := "00000100";
  constant opflagbit_srcm0 : natural                                       := 2;
  constant opflag_srcm1    : std_logic_vector((opflag_width - 1) downto 0) := "00001000";
  constant opflagbit_srcm1 : natural                                       := 3;
  constant opflag_srcm2    : std_logic_vector((opflag_width - 1) downto 0) := "00010000";
  constant opflagbit_srcm2 : natural                                       := 4;



-- selection of math lut
  constant lutsel_none : std_logic_vector((lut_sel_width - 1) downto 0) := "0000";
  constant lutsel_cos  : std_logic_vector((lut_sel_width - 1) downto 0) := "0001";
  constant lutsel_sin  : std_logic_vector((lut_sel_width - 1) downto 0) := "0010";
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
    write(msg, string'("dispsig : "));
    write(msg, name);
    write(msg, string'("("));
    write(msg, ind);
    write(msg, string'(") = "));
    write(msg, val);

    report msg.all;
  end dispsig;

  function "or" (a, b : t_dsp_bus) return t_dsp_bus is
    variable y : t_dsp_bus;
  begin
      -- y <= a or b;
    y.op_done        := a.op_done        or b.op_done       ;
    y.data_out_m0    := a.data_out_m0    or b.data_out_m0   ;
    y.addr_r_m0      := a.addr_r_m0      or b.addr_r_m0     ;
    y.addr_w_m0      := a.addr_w_m0      or b.addr_w_m0     ;
    y.wr_en_m0       := a.wr_en_m0       or b.wr_en_m0      ;
    y.c_en_m0        := a.c_en_m0        or b.c_en_m0       ;
    y.data_out_m1    := a.data_out_m1    or b.data_out_m1   ;
    y.addr_m1        := a.addr_m1        or b.addr_m1       ;
    y.wr_en_m1       := a.wr_en_m1       or b.wr_en_m1      ;
    y.c_en_m1        := a.c_en_m1        or b.c_en_m1       ;
    y.data_out_m2    := a.data_out_m2    or b.data_out_m2   ;
    y.addr_m2        := a.addr_m2        or b.addr_m2       ;
    y.wr_en_m2       := a.wr_en_m2       or b.wr_en_m2      ;
    y.c_en_m2        := a.c_en_m2        or b.c_en_m2       ;
    y.mul_in_a1      := a.mul_in_a1      or b.mul_in_a1     ;
    y.mul_in_b1      := a.mul_in_b1      or b.mul_in_b1     ;
    y.mul_in_a2      := a.mul_in_a2      or b.mul_in_a2     ;
    y.mul_in_b2      := a.mul_in_b2      or b.mul_in_b2     ;
    y.acc_mode1      := a.acc_mode1      or b.acc_mode1     ;
    y.acc_mode2      := a.acc_mode2      or b.acc_mode2     ;
    y.alu_select     := a.alu_select     or b.alu_select    ;
    y.cmp_mode       := a.cmp_mode       or b.cmp_mode      ;
    y.cmp_pol        := a.cmp_pol        or b.cmp_pol       ;
    y.cmp_store      := a.cmp_store      or b.cmp_store     ;
    y.gcounter_reset := a.gcounter_reset or b.gcounter_reset;
    y.lut_in         := a.lut_in         or b.lut_in        ;
    y.lut_select     := a.lut_select     or b.lut_select    ;

    return y;
  end "or";

  function "and" (a : std_logic_vector; b : std_logic) return std_logic_vector is
    constant L : natural := a'length;
    alias aa : std_logic_vector((L - 1) downto 0) is A;
    variable yy : std_logic_vector((L - 1) downto 0);
  begin
    for i in L-1 downto 0 loop
      yy(i) := aa(i) and b;
    end loop;
    return yy;
  end "and";


end dspunit_pac;

