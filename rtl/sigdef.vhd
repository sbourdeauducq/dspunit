-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-------------------------------------------------------------------------------
-- definition du signal MCFull64
-------------------------------------------------------------------------------
package sigdef_pack is

  constant MaxLen : integer := 64;
  type     TTable is array (0 to MaxLen-1) of std_logic_vector(15 downto 0);
  constant sigdef : TTable := (                --Look-up table
    std_logic_vector(to_unsigned(14358, 16)),  --0
    std_logic_vector(to_unsigned(9075, 16)),   --1
    std_logic_vector(to_unsigned(14774, 16)),  --2
    std_logic_vector(to_unsigned(11932, 16)),  --3
    std_logic_vector(to_unsigned(15477, 16)),  --4
    std_logic_vector(to_unsigned(13548, 16)),  --5
    std_logic_vector(to_unsigned(14813, 16)),  --6
    std_logic_vector(to_unsigned(12155, 16)),  --7
    std_logic_vector(to_unsigned(10746, 16)),  --8
    std_logic_vector(to_unsigned(6679, 16)),   --9
    std_logic_vector(to_unsigned(3735, 16)),   --10
    std_logic_vector(to_unsigned(462, 16)),    --11
    std_logic_vector(to_unsigned(48, 16)),     --12
    std_logic_vector(to_unsigned(1767, 16)),   --13
    std_logic_vector(to_unsigned(6605, 16)),   --14
    std_logic_vector(to_unsigned(11714, 16)),  --15
    std_logic_vector(to_unsigned(14995, 16)),  --16
    std_logic_vector(to_unsigned(13371, 16)),  --17
    std_logic_vector(to_unsigned(7730, 16)),   --18
    std_logic_vector(to_unsigned(1495, 16)),   --19
    std_logic_vector(to_unsigned(307, 16)),    --20
    std_logic_vector(to_unsigned(5662, 16)),   --21
    std_logic_vector(to_unsigned(13117, 16)),  --22
    std_logic_vector(to_unsigned(14501, 16)),  --23
    std_logic_vector(to_unsigned(7697, 16)),   --24
    std_logic_vector(to_unsigned(455, 16)),    --25
    std_logic_vector(to_unsigned(2700, 16)),   --26
    std_logic_vector(to_unsigned(11890, 16)),  --27
    std_logic_vector(to_unsigned(14471, 16)),  --28
    std_logic_vector(to_unsigned(5706, 16)),   --29
    std_logic_vector(to_unsigned(0, 16)),      --30
    std_logic_vector(to_unsigned(7879, 16)),   --31
    std_logic_vector(to_unsigned(15082, 16)),  --32
    std_logic_vector(to_unsigned(7143, 16)),   --33
    std_logic_vector(to_unsigned(0, 16)),      --34
    std_logic_vector(to_unsigned(9316, 16)),   --35
    std_logic_vector(to_unsigned(14471, 16)),  --36
    std_logic_vector(to_unsigned(3132, 16)),   --37
    std_logic_vector(to_unsigned(2700, 16)),   --38
    std_logic_vector(to_unsigned(14568, 16)),  --39
    std_logic_vector(to_unsigned(7697, 16)),   --40
    std_logic_vector(to_unsigned(522, 16)),    --41
    std_logic_vector(to_unsigned(13117, 16)),  --42
    std_logic_vector(to_unsigned(9360, 16)),   --43
    std_logic_vector(to_unsigned(307, 16)),    --44
    std_logic_vector(to_unsigned(13527, 16)),  --45
    std_logic_vector(to_unsigned(7730, 16)),   --46
    std_logic_vector(to_unsigned(1652, 16)),   --47
    std_logic_vector(to_unsigned(14995, 16)),  --48
    std_logic_vector(to_unsigned(3308, 16)),   --49
    std_logic_vector(to_unsigned(6605, 16)),   --50
    std_logic_vector(to_unsigned(13256, 16)),  --51
    std_logic_vector(to_unsigned(48, 16)),     --52
    std_logic_vector(to_unsigned(14560, 16)),  --53
    std_logic_vector(to_unsigned(3735, 16)),   --54
    std_logic_vector(to_unsigned(8344, 16)),   --55
    std_logic_vector(to_unsigned(10746, 16)),  --56
    std_logic_vector(to_unsigned(2867, 16)),   --57
    std_logic_vector(to_unsigned(14813, 16)),  --58
    std_logic_vector(to_unsigned(1474, 16)),   --59
    std_logic_vector(to_unsigned(15477, 16)),  --60
    std_logic_vector(to_unsigned(3090, 16)),   --61
    std_logic_vector(to_unsigned(14774, 16)),  --62
    std_logic_vector(to_unsigned(5948, 16)));  --63
end sigdef_pack;

package body sigdef_pack is

end sigdef_pack;

