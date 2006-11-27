-- A global test bench for the project

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.dspalu_pac.all;
-------------------------------------------------------------------------------

entity bench_dspalu is
end bench_dspalu;
--=----------------------------------------------------------------------------
architecture archi_bench_dspalu of bench_dspalu is
  -----------------------------------------------------------------------------
  -- @constants definition
  -----------------------------------------------------------------------------
    constant sig_width               : integer := 16;
    constant acc_width               : integer := 40;
  --=--------------------------------------------------------------------------
  --
  -- @component declarations
  --
  -----------------------------------------------------------------------------
  component clock_gen
    generic (
     tpw : time;
     tps : time
	);
    port (
      clk                      : out std_logic;
      reset                    : out std_logic
	);
  end component;
  component dspalu_acc
    generic (
      sig_width               : integer ;
      acc_width		    : integer
	);
    port (
      a1                       : in std_logic_vector((sig_width - 1) downto 0);
      b1                       : in std_logic_vector((sig_width - 1) downto 0);
      a2                       : in std_logic_vector((sig_width - 1) downto 0);
      b2                       : in std_logic_vector((sig_width - 1) downto 0);
      clk                      : in std_logic;
      clr_acc                  : in std_logic;
      acc_mode1                : in t_acc_mode;
      acc_mode2                : in t_acc_mode;
      alu_select               : in t_alu_select;
      result1                  : out std_logic_vector((sig_width - 1) downto 0);
      result_acc1              : out std_logic_vector((acc_width - 1) downto 0);
      result2                  : out std_logic_vector((sig_width - 1) downto 0);
      result_acc2              : out std_logic_vector((acc_width - 1) downto 0)
	);
  end component;
  --=--------------------------------------------------------------------------
  -- @signals definition
  -----------------------------------------------------------------------------
  signal s_reset             : std_logic;
  signal s_a1                : std_logic_vector((sig_width - 1) downto 0);
  signal s_b1                : std_logic_vector((sig_width - 1) downto 0);
  signal s_a2                : std_logic_vector((sig_width - 1) downto 0);
  signal s_b2                : std_logic_vector((sig_width - 1) downto 0);
  signal s_clk               : std_logic;
  signal s_clr_acc           : std_logic;
  signal s_acc_mode1         : t_acc_mode;
  signal s_acc_mode2         : t_acc_mode;
  signal s_alu_select        : t_alu_select;
  signal s_result1           : std_logic_vector((sig_width - 1) downto 0);
  signal s_result_acc1       : std_logic_vector((acc_width - 1) downto 0);
  signal s_result2           : std_logic_vector((sig_width - 1) downto 0);
  signal s_result_acc2       : std_logic_vector((acc_width - 1) downto 0);
begin  -- archs_bench_dspalu
  -----------------------------------------------------------------------------
  --
  -- @instantiations
  --
  -----------------------------------------------------------------------------
  clock_gen_1 : clock_gen
    generic map (
	  tpw 	=> 5 ns,
	  tps 	=> 0 ns)
    port map (
	  clk 	=> s_clk,
	  reset 	=> s_reset);

  dspalu_acc_1 : dspalu_acc
    generic map (
	  sig_width 	=> sig_width,
	  acc_width 	=> acc_width)
    port map (
	  a1 	=> s_a1,
	  b1 	=> s_b1,
	  a2 	=> s_a2,
	  b2 	=> s_b2,
	  clk 	=> s_clk,
	  clr_acc 	=> s_clr_acc,
	  acc_mode1 	=> s_acc_mode1,
	  acc_mode2 	=> s_acc_mode2,
	  alu_select 	=> s_alu_select,
	  result1 	=> s_result1,
	  result_acc1 	=> s_result_acc1,
	  result2 	=> s_result2,
	  result_acc2 	=> s_result_acc2);

  --=---------------------------------------------------------------------------
  --=---------------------------------------------------------------------------
  --
  -- @concurrent signal assignments
  --
  -----------------------------------------------------------------------------
   s_a1               <= "0010000100000000", "0000000000000010" after 501 ns;
   s_b1               <= "1111111111111011", "0000000000000011" after 501 ns;
   s_a2               <= "0000000000000100";
   s_b2               <= "1111111111111110";
   s_clr_acc         <= not s_reset;
   s_acc_mode1       <= acc_add, acc_store after 201 ns, acc_sub after 301 ns, acc_sumstore after 401 ns,
			acc_store after 501 ns, acc_sub after 701 ns, acc_add after 901 ns;
   s_acc_mode2       <= acc_add, acc_store after 501 ns, acc_sub after 701 ns, acc_add after 901 ns;
   s_alu_select      <= alu_mul, alu_cmul after 501 ns;
 end archi_bench_dspalu;
-------------------------------------------------------------------------------
