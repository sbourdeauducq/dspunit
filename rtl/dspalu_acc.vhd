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
use work.dspalu_pac.all;
-------------------------------------------------------------------------------

entity dspalu_acc is
  generic (
    sig_width               : integer := 16;
    acc_width		    : integer := 32);
  port (
    --@inputs
    a1                       : in std_logic_vector((sig_width - 1) downto 0);
    b1                       : in std_logic_vector((sig_width - 1) downto 0);
    a2                       : in std_logic_vector((sig_width - 1) downto 0);
    b2                       : in std_logic_vector((sig_width - 1) downto 0);
    clk                      : in std_logic;
    clr_acc                  : in std_logic;
    acc_mode1                : in t_acc_mode;
    acc_mode2                : in t_acc_mode;
    alu_select               : in t_alu_select;
    --@outputs
    result1                  : out std_logic_vector((sig_width - 1) downto 0);
    result_acc1              : out std_logic_vector((acc_width - 1) downto 0);
    result2                  : out std_logic_vector((sig_width - 1) downto 0);
    result_acc2              : out std_logic_vector((acc_width - 1) downto 0)
);
end dspalu_acc;
--=----------------------------------------------------------------------------
architecture archi_dspalu_acc of dspalu_acc is
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
  signal s_result1          : signed((2*sig_width - 1) downto 0);
  signal s_result2          : signed((2*sig_width - 1) downto 0);
  signal s_mul_out1          : signed((2*sig_width - 1) downto 0);
  signal s_mul_out2          : signed((2*sig_width - 1) downto 0);
  signal s_result_acc1      : signed((acc_width - 1) downto 0);
  signal s_result_acc2      : signed((acc_width - 1) downto 0);
  signal s_acc_mode1         : t_acc_mode;
  signal s_acc_mode2         : t_acc_mode;
  signal s_acc_mode1_inreg   : t_acc_mode;
  signal s_acc_mode2_inreg   : t_acc_mode;
  signal s_cmul_acc_mode1    : t_acc_mode;
  signal s_cmul_acc_mode2    : t_acc_mode;
  signal s_mul_a1            : std_logic_vector((sig_width - 1) downto 0);
  signal s_mul_a2            : std_logic_vector((sig_width - 1) downto 0);
  signal s_mul_b1            : std_logic_vector((sig_width - 1) downto 0);
  signal s_mul_b2            : std_logic_vector((sig_width - 1) downto 0);
  signal s_mul_a1_in            : std_logic_vector((sig_width - 1) downto 0);
  signal s_mul_a2_in            : std_logic_vector((sig_width - 1) downto 0);
  signal s_mul_b1_in            : std_logic_vector((sig_width - 1) downto 0);
  signal s_mul_b2_in            : std_logic_vector((sig_width - 1) downto 0);
  type t_cmul_state is (cmul_step, cmul_end);
  signal s_cmul_state        : t_cmul_state;
  signal s_b2                : std_logic_vector((sig_width - 1) downto 0);
begin  -- archs_dspalu_acc
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  -------------------------------------------------------------------------------
  -- First accumulator
  -------------------------------------------------------------------------------
  p_acc1 : process (clk)
   variable v_tmp_acc1 : signed((acc_width - 1) downto 0);
  begin -- process p_acc
    if rising_edge(clk) then  -- rising clock edge
      if(clr_acc = '1') then
        s_result_acc1 <= (others => '0');
      else
        v_tmp_acc1 := resize(s_result1, acc_width);

        -- Accumulation mode
	case s_acc_mode1 is
	  when acc_store =>
	    s_result_acc1 <= v_tmp_acc1;
	  when acc_sumstore =>
	    s_result_acc1 <= resize(signed(s_result1) + signed(s_result2), acc_width);
	  when acc_add =>
	    s_result_acc1 <= s_result_acc1 + v_tmp_acc1;
	  when acc_sub =>
	    s_result_acc1 <= s_result_acc1 - v_tmp_acc1;
	  when others =>
	    s_result_acc1 <= (others => '0');
	end case;
      end if;
    end if;
  end process p_acc1;
  -------------------------------------------------------------------------------
  -- Second accumulator
  -------------------------------------------------------------------------------
  p_acc2 : process (clk)
   variable v_tmp_acc2 : signed((acc_width - 1) downto 0);
  begin -- process p_acc
    if rising_edge(clk) then  -- rising clock edge
      if(clr_acc = '1') then
        s_result_acc2 <= (others => '0');
      else
        v_tmp_acc2 := resize(s_result2, acc_width);

        -- Accumulation mode
	case s_acc_mode2 is
	  when acc_store =>
	    s_result_acc2 <= v_tmp_acc2;
	  when acc_diff =>
--	    s_result_acc2 <= resize(signed(a2) + signed(b2), acc_width);
	    s_result_acc2 <= s_result_acc2 - s_result_acc1;
	  when acc_add =>
	    s_result_acc2 <= s_result_acc2 + v_tmp_acc2;
	  when acc_sub =>
	    s_result_acc2 <= s_result_acc2 - v_tmp_acc2;
	  when others =>
	    s_result_acc2 <= (others => '0');
	end case;
      end if;
    end if;
  end process p_acc2;
  -------------------------------------------------------------------------------
  -- Operation controller (manage the complex multiplication)
  -------------------------------------------------------------------------------
  p_alu_ctrl : process (clk)
  begin -- process p_alu_ctrl
    if rising_edge(clk) then  -- rising clock edge
      if (alu_select = alu_mul) then
        s_cmul_state <= cmul_end;
      elsif (s_cmul_state = cmul_step) then
        s_cmul_state <= cmul_end;
      else
        s_cmul_state <= cmul_step;
      end if;
    end if;
  end process p_alu_ctrl;

  p_mul_reg : process (clk)
  begin -- process p_mul_reg
    if rising_edge(clk) then  -- rising clock edge
      s_result1 <= s_mul_out1;
      s_result2 <= s_mul_out2;
      s_acc_mode1 <= s_acc_mode1_inreg;
      s_acc_mode2 <= s_acc_mode2_inreg;
    end if;
  end process p_mul_reg;
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
  result1           <= std_logic_vector(s_result1((2*sig_width - 2) downto (sig_width - 1)));
  result2           <= std_logic_vector(s_result2((2*sig_width - 2) downto (sig_width - 1)));
  s_mul_out1       <= signed(s_mul_a1) * signed(s_mul_b1);
  s_mul_out2       <= signed(s_mul_a2) * signed(s_mul_b2);
--  s_mul_out1       <= signed(s_mul_a1(sig_width -1) & s_mul_a1 & zeros(sig_width - 1)) when s_mul_b1 = sig_one(sig_width) else
--		      signed(s_mul_b1(sig_width -1) & s_mul_b1 & zeros(sig_width - 1)) when s_mul_a1 = sig_one(sig_width) else
--		      signed(s_mul_a1) * signed(s_mul_b1);
--  s_mul_out2       <= signed(s_mul_a2(sig_width -1) & s_mul_a2 & zeros(sig_width - 1)) when s_mul_b2 = sig_one(sig_width) else
--		      signed(s_mul_b2(sig_width -1) & s_mul_b2 & zeros(sig_width - 1)) when s_mul_a2 = sig_one(sig_width) else
--		      signed(s_mul_a2) * signed(s_mul_b2);
  result_acc1        <= std_logic_vector(s_result_acc1);
  result_acc2        <= std_logic_vector(s_result_acc2);

  -- accumulation mode is given by acc_modex except during complex multiplication (modified for step 2)
  s_cmul_acc_mode1   <= acc_add when acc_mode1 = acc_sub else acc_sub;
  s_cmul_acc_mode2   <= acc_sub when acc_mode1 = acc_sub else acc_add;
  s_acc_mode1_inreg  <= s_cmul_acc_mode1 when s_cmul_state = cmul_step else acc_mode1;
  s_acc_mode2_inreg  <= s_cmul_acc_mode2 when s_cmul_state = cmul_step else acc_mode2;

  -- multipliers inputs (special selection during complex multiplication)
  p_mul_in_reg : process (clk)
  begin -- process p_mul_reg
    if rising_edge(clk) then  -- rising clock edge
      s_mul_a1           <= s_mul_a1_in;
      s_mul_a2           <= s_mul_a2_in;
      s_mul_b1           <= s_mul_b1_in;
      s_mul_b2           <= s_mul_b2_in;
    end if;
  end process p_mul_in_reg;
  s_mul_a1_in           <= a2 when s_cmul_state = cmul_step else a1;
  s_mul_a2_in           <= a1 when s_cmul_state = cmul_step else a2;
  s_mul_b1_in           <= s_b2 when s_cmul_state = cmul_step else b1;
  -- ! can be more time critical than other entries because depends on alu_select
  s_mul_b2_in           <= b1 when (s_cmul_state = cmul_end and (alu_select = alu_cmul or alu_select = alu_cmul_conj)) else s_b2;
  -- ------------------------------------------------------------------------------------------------------------------------
  s_b2               <= std_logic_vector(-signed(b2)) when alu_select = alu_cmul_conj else b2;
end archi_dspalu_acc;
-------------------------------------------------------------------------------
