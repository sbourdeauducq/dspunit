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
--
--   Pipelined divider usign restoring algorithm
--   total pipe length is (sig_width + 1)
---------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-------------------------------------------------------------------------------

entity dspdiv is
  generic (
    sig_width : integer := 16);
  port (
    --@inputs
    num         : in  std_logic_vector((2*sig_width - 1) downto 0);
    den         : in  std_logic_vector((sig_width - 1) downto 0);
    clk         : in  std_logic;
    --@outputs;
  q                          : out std_logic_vector((sig_width - 1) downto 0);
  r                          : out std_logic_vector((2*sig_width - 3) downto 0)
);
end dspdiv;
--=----------------------------------------------------------------------------
architecture archi_dspdiv of dspdiv is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
  constant c_div_length                : integer := sig_width - 1;
  constant c_work_length               : integer := 2*c_div_length;
  constant c_trial_length              : integer := c_div_length + 2;
  constant c_trial_overflow            : integer := c_trial_length - 2;
  constant c_trial_sign                : integer := c_trial_length - 1;
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  type     t_work_pipe is array(0 to c_div_length) of std_logic_vector((c_work_length - 1) downto 0);
  type     t_trial_pipe is array(0 to c_div_length) of unsigned((c_trial_length - 1) downto 0);
  --type     t_trial_pipe is array(0 to c_div_length) of std_logic_vector((c_trial_length - 1) downto 0);
  type     t_val_pipe is array(0 to c_div_length - 1) of std_logic_vector((c_div_length - 1) downto 0);
  type     t_bit_pipe is array(0 to c_div_length - 1) of std_logic;
  signal s_r                 : t_work_pipe;
  signal s_trial_r           : t_trial_pipe;
  signal s_d                 : t_val_pipe;
  signal s_q                 : t_val_pipe;
  signal s_sign              : t_bit_pipe;
  signal s_overflow          : t_bit_pipe;
  signal s_overflow_cur      : t_bit_pipe;
  signal s_num_abs           : signed((2*sig_width - 1) downto 0);
  signal s_num_sign          : std_logic;
  signal s_den_abs           : signed((sig_width - 1) downto 0);
  signal s_den_sign          : std_logic;
  signal s_sign_last         : std_logic;
  --
  -- SIgnals for debug
  --
  signal s_d0   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d1   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d2   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d3   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d4   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d5   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d6   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d7   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d8   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d9   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d10  : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d11  : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d12  : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d13  : std_logic_vector((c_div_length - 1) downto 0);
  signal s_d14  : std_logic_vector((c_div_length - 1) downto 0);
  --signal s_d15  : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q0   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q1   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q2   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q3   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q4   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q5   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q6   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q7   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q8   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q9   : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q10  : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q11  : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q12  : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q13  : std_logic_vector((c_div_length - 1) downto 0);
  signal s_q14  : std_logic_vector((c_div_length - 1) downto 0);
  --signal s_q15  : std_logic_vector((c_div_length - 1) downto 0);
  signal s_r0   : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r1   : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r2   : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r3   : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r4   : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r5   : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r6   : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r7   : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r8   : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r9   : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r10  : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r11  : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r12  : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r13  : std_logic_vector((c_work_length - 1) downto 0);
  signal s_r14  : std_logic_vector((c_work_length - 1) downto 0);
  --signal s_r15  : std_logic_vector((c_work_length - 1) downto 0);
  signal s_trial_r0   : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r1   : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r2   : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r3   : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r4   : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r5   : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r6   : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r7   : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r8   : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r9   : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r10  : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r11  : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r12  : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r13  : unsigned((c_trial_length - 1) downto 0);
  signal s_trial_r14  : unsigned((c_trial_length - 1) downto 0);
  --signal s_trial_r15  : unsigned((c_trial_length - 1) downto 0);

begin  -- archs_dspdiv
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  p_div : process (clk)
  begin -- process p_div
    if rising_edge(clk) then  -- rising clock edge
      -- s_r(0)((c_div_length - 1) downto 0)<= num(2*sig_width - 2 downto (2*sig_width - c_div_length - 1));
      -- s_r(0)((c_work_length - 1) downto c_div_length) <= (others => '0');
      s_r(0) <= std_logic_vector(s_num_abs((c_work_length - 1) downto 0));
      s_d(0) <= std_logic_vector(s_den_abs((c_div_length - 1) downto 0));
      s_sign(0) <= s_num_sign xor s_den_sign;
      -- fill unused lines with 0 for simulation
      for i in 0 to c_div_length - 2 loop
        -- propagation of quotient bits previously computed
        s_q(i)((c_div_length - i - 2) downto 0) <= (others => '0');
      end loop;
      -- pipe
      for i in 1 to c_div_length - 1 loop
        s_sign(i) <= s_sign(i - 1);
        s_d(i) <= s_d(i - 1);
        -- propagation of quotient bits previously computed
        s_q(i)((c_div_length - 1) downto (c_div_length - i)) <= s_q(i - 1)((c_div_length - 1) downto (c_div_length - i));
        -- test for overflow (denominator too small)
        s_overflow(i) <= s_overflow(i - 1) or s_overflow_cur(i);
      end loop;
      s_overflow(0) <= s_overflow_cur(0);
      s_sign_last <= s_sign(c_div_length - 1);

      for i in 0 to c_div_length - 1 loop
        if s_trial_r(i)(c_trial_length - 1) = '0' then --if >= 0
          s_r(i + 1)((c_work_length - 1) downto c_div_length) <= std_logic_vector(s_trial_r(i)(c_div_length - 1 downto 0));   -- store trial reminder
          s_q(i)(c_div_length - 1 - i) <= '1';
        else
          -- restore s_r and shift one bit left (R <- 2R)
          s_r(i + 1)((c_work_length - 1) downto c_div_length) <= s_r(i)(c_work_length - 2 downto c_div_length - 1);
          s_q(i)(c_div_length - 1 - i) <= '0';
        end if;
        -- The lower part of the remainder is just shifted
        s_r(i + 1)((c_div_length - 1) downto 0) <= s_r(i)((c_div_length - 2) downto 0) & '0';
      end loop;

    end if;
  end process p_div;
  p_sign : process (num,den)
  begin -- process p_sign
    if den(sig_width - 1) = '0' then
      s_den_abs <= signed(den);
      s_den_sign <= '0';
    else
      s_den_abs <= -signed(den);
      s_den_sign <= '1';
    end if;
    if num(2*sig_width - 1) = '0' then
      s_num_abs <= signed(num);
      s_num_sign <= '0';
    else
      s_num_abs <= -signed(num);
      s_num_sign <= '1';
    end if;
  end process p_sign;
  p_out_reg : process (clk)
  begin -- process p_out_reg
    if rising_edge(clk) then  -- rising clock edge
      if s_overflow(c_div_length - 1) = '1' then
        q(sig_width - 2 downto 0) <= (others => '1');
        q(sig_width - 1) <= '0';
        r <= (others => '0');
      elsif s_sign_last = '0' then
        r <= s_r(c_div_length - 1);
        q <= '0' & s_q(c_div_length - 1);
      else
        r <= s_r(c_div_length - 1);
        q <= std_logic_vector(-signed('0' & s_q(c_div_length - 1)));
      end if;
    end if;
  end process p_out_reg;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  computtrial : for i in 0 to c_div_length - 1 generate
    -- compute the trial reminder (substractions) (Rt <- 2R - D)
    -- substract performed only on the left part of s_r
--    s_trial_r(i) <= std_logic_vector(
--            unsigned('0' & std_logic_vector(s_r(i)((c_work_length - 1) downto (c_work_length - c_div_length - 1))))
--            - unsigned("00" & std_logic_vector(s_d(i))));
    s_trial_r(i) <=
            ('0' & unsigned(s_r(i)((c_work_length - 1) downto (c_work_length - c_div_length - 1))))
            - ("00" & unsigned(s_d(i)));
  end generate computtrial;

  overflow_cur : for i in 0 to c_div_length - 1 generate
    s_overflow_cur(i) <= not s_trial_r(i)(c_trial_sign) and s_trial_r(i)(c_trial_overflow);
  end generate overflow_cur;

  ------------------------------------------------------------------------------
  --
  -- Details on signals shift for the computation of trial remainder
  --
  ------------------------------------------------------------------------------
  --
  --  Operation performed : Rtrial(n) = 2R(n - 1) - Den << N
  --
  --                      ----------------------------------------------------------------
  --            bits         29  27  25  23  21  19  17  15  13  11   9   7   5   3   1
  --            numbers:   30  28  26  24  22  20  18  16  14  12  10   8   6   4   2   0
  --                      ----------------------------------------------------------------
  --|
  --|                   |     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  --|   s_r (shifted)   |   0|                            r(n-1)                         |
  --|                   |     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  --|
  --|                   |       - - - - - - - - - - - - - - -
  --|   s_d (shifted)   |   0 0|     denominator             |0 0 0 0 0 0 0 0 0 0 0 0 0 0
  --|                   |       - - - - - - - - - - - - - - -
  --|
  --|                   |   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  --|   s_trial_r       |  |s|o|                                                       |
  --|                   |   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  --
  --
  --   if s = 1 (negative value) : restore previous remainder
  --   if o = 1 and s = 0 : the denominator is too small : quotient is infinity
  --   if o = 0 and s = 0 : the difference is the new remainder : R(n) <= Rtrial(n)
  --


  --
  -- Signals for debug
  --
  s_d0 <= s_d(0);
  s_d1 <= s_d(1);
  s_d2 <= s_d(2);
  s_d3 <= s_d(3);
  s_d4 <= s_d(4);
  s_d5 <= s_d(5);
  s_d6 <= s_d(6);
  s_d7 <= s_d(7);
  s_d8 <= s_d(8);
  s_d9 <= s_d(9);
  s_d10 <= s_d(10);
  s_d11 <= s_d(11);
  s_d12 <= s_d(12);
  s_d13 <= s_d(13);
  s_d14 <= s_d(14);
  --s_d15 <= s_d(15);

  s_q0 <= s_q(0);
  s_q1 <= s_q(1);
  s_q2 <= s_q(2);
  s_q3 <= s_q(3);
  s_q4 <= s_q(4);
  s_q5 <= s_q(5);
  s_q6 <= s_q(6);
  s_q7 <= s_q(7);
  s_q8 <= s_q(8);
  s_q9 <= s_q(9);
  s_q10 <= s_q(10);
  s_q11 <= s_q(11);
  s_q12 <= s_q(12);
  s_q13 <= s_q(13);
  s_q14 <= s_q(14);
  --s_q15 <= s_q(15);

  s_r0 <= s_r(0);
  s_r1 <= s_r(1);
  s_r2 <= s_r(2);
  s_r3 <= s_r(3);
  s_r4 <= s_r(4);
  s_r5 <= s_r(5);
  s_r6 <= s_r(6);
  s_r7 <= s_r(7);
  s_r8 <= s_r(8);
  s_r9 <= s_r(9);
  s_r10 <= s_r(10);
  s_r11 <= s_r(11);
  s_r12 <= s_r(12);
  s_r13 <= s_r(13);
  s_r14 <= s_r(14);
  --s_r15 <= s_r(15);

  s_trial_r0 <= s_trial_r(0);
  s_trial_r1 <= s_trial_r(1);
  s_trial_r2 <= s_trial_r(2);
  s_trial_r3 <= s_trial_r(3);
  s_trial_r4 <= s_trial_r(4);
  s_trial_r5 <= s_trial_r(5);
  s_trial_r6 <= s_trial_r(6);
  s_trial_r7 <= s_trial_r(7);
  s_trial_r8 <= s_trial_r(8);
  s_trial_r9 <= s_trial_r(9);
  s_trial_r10 <= s_trial_r(10);
  s_trial_r11 <= s_trial_r(11);
  s_trial_r12 <= s_trial_r(12);
  s_trial_r13 <= s_trial_r(13);
  s_trial_r14 <= s_trial_r(14);
  --s_trial_r15 <= s_trial_r(15);
 end archi_dspdiv;
-------------------------------------------------------------------------------
