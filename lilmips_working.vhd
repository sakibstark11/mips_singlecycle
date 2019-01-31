-- mipssingle.vhd
-- Single-cycle MIPS processor
-- From Section 7.6 of Digital Design & Computer Architecture

--Single-cycle MIPS processor------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mips is 
  port(clk, reset:        in  STD_LOGIC;
       pc:                buffer STD_LOGIC_VECTOR(31 downto 0);
       instr:             in  STD_LOGIC_VECTOR(31 downto 0);
       memwrite:          buffer STD_LOGIC;
       aluout, writedata: buffer STD_LOGIC_VECTOR(31 downto 0);
       readdata:          in  STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of mips is
  component controller
    port(op, funct:          in  STD_LOGIC_VECTOR(5 downto 0);
         zero,letz:          in  STD_LOGIC;
         memtoreg:           out STD_LOGIC;
         memwrite:           buffer STD_LOGIC;
         pcsrc:              out STD_LOGIC;
         alusrc:             out STD_LOGIC_VECTOR(1 downto 0);
         regdst, regwrite:   out STD_LOGIC;
         jump:               out STD_LOGIC;
         alucontrol:         out STD_LOGIC_VECTOR(3 downto 0));
  end component;
  component datapath
    port(clk, reset:        in  STD_LOGIC;
         memtoreg, pcsrc:   in  STD_LOGIC;
         alusrc:            in  STD_LOGIC_VECTOR(1 downto 0);
         regdst:            in  STD_LOGIC;
         regwrite, jump:    in  STD_LOGIC;
         alucontrol:        in  STD_LOGIC_VECTOR(3 downto 0);
         zero,letz:         out STD_LOGIC;
         shamt:             in STD_LOGIC_VECTOR(4 downto 0);
         pc:                buffer STD_LOGIC_VECTOR(31 downto 0);
         instr:             in STD_LOGIC_VECTOR(31 downto 0);
         aluout, writedata: buffer STD_LOGIC_VECTOR(31 downto 0);
         readdata:          in  STD_LOGIC_VECTOR(31 downto 0));
  end component;
  signal memtoreg, regdst, regwrite, jump, pcsrc: STD_LOGIC;
  signal alusrc: STD_LOGIC_VECTOR(1 downto 0);
  signal zero,letz: STD_LOGIC;
  signal shamt: STD_LOGIC_VECTOR(4 downto 0); -- added shamt feature
  signal alucontrol: STD_LOGIC_VECTOR(3 downto 0);
begin
  cont: controller port map(instr(31 downto 26), instr(5 downto 0),
                            zero, letz, memtoreg, memwrite, pcsrc, alusrc,
                            regdst, regwrite, jump, alucontrol);
  dp: datapath port map(clk, reset, memtoreg, pcsrc, alusrc, regdst,
                        regwrite, jump, alucontrol, zero, letz,instr(10 downto 6), pc, instr,
                        aluout, writedata, readdata);
end;

--Single cycle control decoder-----------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity controller is 
  port(op, funct:          in  STD_LOGIC_VECTOR(5 downto 0);
       zero, letz:         in  STD_LOGIC;
       memtoreg:           out STD_LOGIC;  
       memwrite:           buffer STD_LOGIC;
       pcsrc:              out STD_LOGIC;
       alusrc:             out STD_LOGIC_VECTOR(1 downto 0); --2 bits to accomodate more options
       regdst, regwrite:   out STD_LOGIC;
       jump:               out STD_LOGIC;
       alucontrol:         out STD_LOGIC_VECTOR(3 downto 0));
end;

architecture struct of controller is
  component maindec
    port(op:                 in  STD_LOGIC_VECTOR(5 downto 0);
         memtoreg:           out STD_LOGIC;    
         memwrite:           buffer STD_LOGIC;
         branch, blez:       out STD_LOGIC;
         alusrc:             out STD_LOGIC_VECTOR(1 downto 0); --2 bits to accomodate more options
         regdst, regwrite:   out STD_LOGIC;
         jump:               out STD_LOGIC;
         aluop:              out STD_LOGIC_VECTOR(1 downto 0));
  end component;
  component aludec
    port(funct:      in  STD_LOGIC_VECTOR(5 downto 0);
         aluop:      in  STD_LOGIC_VECTOR(1 downto 0);
         alucontrol: out STD_LOGIC_VECTOR(3 downto 0));
  end component;
  signal aluop:  STD_LOGIC_VECTOR(1 downto 0);
  signal branch, blez: STD_LOGIC;
begin
  md: maindec port map(op, memtoreg, memwrite, branch, blez,
                       alusrc, regdst, regwrite, jump, aluop);
  ad: aludec port map(funct, aluop, alucontrol);

  pcsrc <= (branch and zero) or (blez and letz);
end;

-- Main control decoder------------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity maindec is 
  port(op:                 in  STD_LOGIC_VECTOR(5 downto 0);
       memtoreg:           out STD_LOGIC; 
       memwrite:           buffer STD_LOGIC;
       branch, blez:       out STD_LOGIC;
       alusrc:             out STD_LOGIC_VECTOR(1 downto 0); --2 bits to accomodate more options       
       regdst, regwrite:   out STD_LOGIC;
       jump:               out STD_LOGIC;
       aluop:              out STD_LOGIC_VECTOR(1 downto 0));
end;

architecture behave of maindec is
  signal controls: STD_LOGIC_VECTOR(10 downto 0);
begin
  process(op) begin
    case op is
      when "000000" => controls <= "11000000010"; -- RTYPE
      when "100011" => controls <= "10100001000"; -- LW
      when "101011" => controls <= "00100010000"; -- SW
      when "000100" => controls <= "00001000001"; -- BEQ
      when "001000" => controls <= "10100000000"; -- ADDI
      when "000010" => controls <= "00000000100"; -- J
      when "001111" => controls <= "10010000000"; -- LUI
      when "001010" => controls <= "10100000011"; -- SLTI
      when "000110" => controls <= "00000100001"; -- BLEZ
      when others   => controls <= "-----------"; -- illegal op
    end case;
  end process;  

    regwrite <= controls(10);
    regdst   <= controls(9);
    alusrc   <= controls(8 downto 7);
    branch   <= controls(6);
    blez     <= controls(5);
    memwrite <= controls(4);
    memtoreg <= controls(3);
    jump     <= controls(2);
    aluop    <= controls(1 downto 0);
end;

--ALU control decoder--------------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity aludec is 
  port(funct:      in  STD_LOGIC_VECTOR(5 downto 0);
       aluop:      in  STD_LOGIC_VECTOR(1 downto 0);
       alucontrol: out STD_LOGIC_VECTOR(3 downto 0));
end;

architecture behave of aludec is
begin
  process(aluop, funct) begin
    case aluop is 
      when "00" => alucontrol <= "0010"; -- add (for lw/sw/addi)
      when "01" => alucontrol <= "1010"; -- sub (for beq/BLEZ)
      when "11" => alucontrol <= "1011"; -- sub (for SLTI)
      when others => case funct is      -- R-type instructions
                         when "100000" => alucontrol <= "0010"; -- add 
                         when "100010" => alucontrol <= "1010"; -- sub
                         when "100100" => alucontrol <= "0000"; -- and
                         when "100101" => alucontrol <= "0001"; -- or
                         when "101010" => alucontrol <= "1011"; -- slt
                         when "000000" => alucontrol <= "0100"; -- sll
                         when others   => alucontrol <= "----"; -- ???
                     end case;
    end case;
  end process;
end;

--MIPS datapath--------------------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all; use IEEE.STD_LOGIC_ARITH.all;

entity datapath is  
  port(clk, reset:        in  STD_LOGIC;
       memtoreg, pcsrc:   in  STD_LOGIC;
       alusrc:            in  STD_LOGIC_VECTOR(1 downto 0);--***       
       regdst:            in  STD_LOGIC;
       regwrite, jump:    in  STD_LOGIC;
       alucontrol:        in  STD_LOGIC_VECTOR(3 downto 0);
       shamt:             in STD_LOGIC_VECTOR(4 downto 0);
       zero, letz:        out STD_LOGIC;
       pc:                buffer STD_LOGIC_VECTOR(31 downto 0);
       instr:             in  STD_LOGIC_VECTOR(31 downto 0);
       aluout, writedata: buffer STD_LOGIC_VECTOR(31 downto 0);
       readdata:          in  STD_LOGIC_VECTOR(31 downto 0));
end;

architecture struct of datapath is
  component alu
    port(a, b:       in  STD_LOGIC_VECTOR(31 downto 0);
         alucontrol: in  STD_LOGIC_VECTOR(3 downto 0);
         shamt:             in STD_LOGIC_VECTOR(4 downto 0);
         result:     buffer STD_LOGIC_VECTOR(31 downto 0);
         zero, letz: out STD_LOGIC);
  end component;
  component regfile
    port(clk:           in  STD_LOGIC;
         we3:           in  STD_LOGIC;
         ra1, ra2, wa3: in  STD_LOGIC_VECTOR(4 downto 0);
         wd3:           in  STD_LOGIC_VECTOR(31 downto 0);
         rd1:           out STD_LOGIC_VECTOR(31 downto 0);
         rd2:           buffer STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component adder
    port(a, b: in  STD_LOGIC_VECTOR(31 downto 0);
         y:    out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component sl2
    port(a: in  STD_LOGIC_VECTOR(31 downto 0);
         y: out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component lui        -- this component will shift the 15:0 bits and make them 32 bits
    port(a: in STD_LOGIC_VECTOR(15 downto 0);
         y: out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component signext
    port(a: in  STD_LOGIC_VECTOR(15 downto 0);
         y: out STD_LOGIC_VECTOR(31 downto 0));
  end component;
  component flopr generic(width: integer);
    port(clk, reset: in  STD_LOGIC;
         d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
         q:          buffer STD_LOGIC_VECTOR(width-1 downto 0));
  end component;
  component mux2 generic(width: integer);
    port(d0, d1: in  STD_LOGIC_VECTOR(width-1 downto 0);
         s:      in  STD_LOGIC;
         y:      out STD_LOGIC_VECTOR(width-1 downto 0));
  end component;
  component mux3 generic(width: integer); -- new mux
    port(d0, d1, d2: in STD_LOGIC_VECTOR(width-1 downto 0);
                 s : in STD_LOGIC_VECTOR(1 downto 0);-- select line
                 y:  out STD_LOGIC_VECTOR(width-1 downto 0));
    end component;
  signal writereg:           STD_LOGIC_VECTOR(4 downto 0);
  signal pcjump, pcnext, 
         pcnextbr, pcplus4, 
         pcbranch:           STD_LOGIC_VECTOR(31 downto 0);
  signal signimm, signimmsh: STD_LOGIC_VECTOR(31 downto 0);
  signal srca, srcb, result: STD_LOGIC_VECTOR(31 downto 0);
  signal luiimm:             STD_LOGIC_VECTOR(31 downto 0);

begin
  -- next PC logic
  pcjump <= pcplus4(31 downto 28) & instr(25 downto 0) & "00";
  pcreg: flopr generic map(32) port map(clk, reset, pcnext, pc);
  pcadd1: adder port map(pc, X"00000004", pcplus4);
  immsh: sl2 port map(signimm, signimmsh);
  pcadd2: adder port map(pcplus4, signimmsh, pcbranch);
  pcbrmux: mux2 generic map(32) port map(pcplus4, pcbranch, 
                                         pcsrc, pcnextbr);
  luish: lui port map(instr(15 downto 0), luiimm); --shifting                                                                               
  pcmux: mux2 generic map(32) port map(pcnextbr, pcjump, jump, pcnext);

  -- register file logic
  rf: regfile port map(clk, regwrite, instr(25 downto 21), 
                       instr(20 downto 16), writereg, result, srca, 
				writedata);
  wrmux: mux2 generic map(5) port map(instr(20 downto 16), 
                                      instr(15 downto 11), 
                                      regdst, writereg);
  resmux: mux2 generic map(32) port map(aluout, readdata, 
                                        memtoreg, result);
  se: signext port map(instr(15 downto 0), signimm);

  -- ALU logic
  srcbmux: mux3 generic map(32) port map(writedata, signimm, luiimm, alusrc, 
                                         srcb);
  mainalu: alu port map(srca, srcb, alucontrol,shamt, aluout, zero, letz);
end;

--Three-port register file---------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.STD_LOGIC_UNSIGNED.all;

entity regfile is 
  port(clk:           in  STD_LOGIC;
       we3:           in  STD_LOGIC;
       ra1, ra2, wa3: in  STD_LOGIC_VECTOR(4 downto 0);
       wd3:           in  STD_LOGIC_VECTOR(31 downto 0);
       rd1:           out STD_LOGIC_VECTOR(31 downto 0);
       rd2:           buffer STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of regfile is
  type ramtype is array (31 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
  signal mem: ramtype;
begin
  -- three-ported register file
  -- read two ports combinationally
  -- write third port on rising edge of clock
  -- register 0 hardwired to 0
  -- note: for pipelined processor, write third port
  -- on falling edge of clk
  process(clk) begin
    if clk'event and clk = '1' then
       if we3 = '1' then mem(CONV_INTEGER(wa3)) <= wd3;
       end if;
    end if;
  end process;
  process(ra1, ra2) begin
    if (CONV_INTEGER(ra1) = 0) then rd1 <= X"00000000"; -- register 0 holds 0
    else rd1 <= mem(CONV_INTEGER(ra1));
    end if;
    if (CONV_INTEGER(ra2) = 0) then rd2 <= X"00000000"; 
    else rd2 <= mem(CONV_INTEGER(ra2));
    end if;
  end process;
end;

--Adder----------------------------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.STD_LOGIC_UNSIGNED.all;

entity adder is
  port(a, b: in  STD_LOGIC_VECTOR(31 downto 0);
       y:    out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of adder is
begin
  y <= a + b;
end;

-- lui adder for lui feature
library IEEE; use IEEE.STD_LOGIC_1164.all;

entity lui is 
  port(a: in  STD_LOGIC_VECTOR(15 downto 0);
       y: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of lui is
begin
  y <= a & X"0000";   -- x for hex
end;


--Shift left by 2 (Multiply by 4)--------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity sl2 is 
  port(a: in  STD_LOGIC_VECTOR(31 downto 0);
       y: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of sl2 is
begin
  y <= a(29 downto 0) & "00";
end;



--Sign extender--------------------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity signext is 
  port(a: in  STD_LOGIC_VECTOR(15 downto 0);
       y: out STD_LOGIC_VECTOR(31 downto 0));
end;

architecture behave of signext is
begin
  y <= X"ffff" & a when a(15) = '1' else X"0000" & a; 
end;

--Resettable flip-flop-------------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity flopr is
  generic(width: integer);
  port(clk, reset: in  STD_LOGIC;
       d:          in  STD_LOGIC_VECTOR(width-1 downto 0);
       q:          buffer STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture asynchronous of flopr is
begin
  process(clk, reset) begin
    if reset = '1' then  q <= (others => '0');
    elsif clk'event and clk = '1' then
      q <= d;
    end if;
  end process;
end;

--2:1 multiplexer------------------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mux2 is 
  generic(width: integer);
  port(d0, d1: in  STD_LOGIC_VECTOR(width-1 downto 0);
       s:      in  STD_LOGIC;
       y:      out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture behave of mux2 is
begin
  y <= d1 when s = '1' else d0;
end;

-- 3:1 mux3 the new mux

library IEEE; use IEEE.STD_LOGIC_1164.all;

entity mux3 is 
  generic(width: integer);
  port(d0, d1, d2: in  STD_LOGIC_VECTOR(width-1 downto 0);
       s:      in  STD_LOGIC_VECTOR(1 downto 0);
       y:      out STD_LOGIC_VECTOR(width-1 downto 0));
end;

architecture behave of mux3 is
begin
  y <= d0 when s = "00"
  else d1 when s = "10"
  else d2 when s = "01";
  end;


--Arithmetic/Logic unit with add/sub, AND, OR, set less than-----------------------

library IEEE; use IEEE.STD_LOGIC_1164.all; 
use IEEE.STD_LOGIC_UNSIGNED.all;
use ieee.numeric_std.all; -- new lib

entity alu is  
  port(a, b:       in  STD_LOGIC_VECTOR(31 downto 0);
       alucontrol: in  STD_LOGIC_VECTOR(3 downto 0);
       shamt:             in STD_LOGIC_VECTOR(4 downto 0);
       result:     buffer STD_LOGIC_VECTOR(31 downto 0);
       zero, letz: out STD_LOGIC);
end;

architecture behave of alu is
  signal condinvb,shifted, sum: STD_LOGIC_VECTOR(31 downto 0);
begin
  condinvb <= not b when alucontrol(3) = '1' else b;
  shifted <= std_logic_vector(unsigned(condinvb) sll to_integer(unsigned(shamt)));
 sum <= a + condinvb + alucontrol(3);

  process(alucontrol, a, b, sum) begin
    case alucontrol(2 downto 0) is
      when "000"   => result <= a and b; 
      when "001"   => result <= a or b; 
      when "010"   => result <= sum;
      when "100"   => result <= shifted; 
      -- slt should be 1 if most significant bit of sum is 1
      when "011"   => result <= (0 => sum(31), others => '0'); 
      when others => result <= (others => 'X'); 
    end case;
  end process;

  zero <= '1' when result = X"00000000" else '0';
  letz <= '1' when (result = X"00000000" or a(31)= '1') else '0';
    
end;