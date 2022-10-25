--DF
--ULA
entity alu_1_bit is --ULA 1 BIT
    port (
      A,B: in bit;
      Ci: in bit;
      S: in bit_vector(3 downto 0);
      F: out bit;
      Co: out bit;
      SumOut: out bit
    ) ;
  end alu_1_bit;

  architecture arch of alu_1_bit is
      signal x, y: bit;
      begin

         x <= A when S(3) = '0' else not A;
         y <= B when S(2) = '0' else not B;
          
          with S(1 downto 0) select F <=
              x and y when "00",
              x or y when "01",
              x xor y xor Ci when others;
  
          Co <= ((x and y) or ((Ci and y) or (x and Ci)));

          SumOut <= (x xor y xor Ci);
          
end architecture ;

library ieee;
use ieee.numeric_bit.all;
entity alu is
    generic (
        size: natural := 64
    );
    port (
        A, B: in bit_vector(size - 1 downto 0); 
        F: out bit_vector(size - 1 downto 0); 
        S: in bit_vector(3 downto 0); 
        Z: out bit; 
        Ov: out bit; 
        Co: out bit 
    );
end entity alu;

architecture arch_alu of alu is
    component alu_1_bit is
        port (
            A,B: in bit;
            Ci: in bit;
            S: in bit_vector(3 downto 0);
            F: out bit;
            Co: out bit;
            SumOut: out bit
           
        );
    end component;


    signal vetorCarry: bit_vector(size-1 downto 0);
    signal primeiroCarry: bit;
    signal resultado: bit_vector(size - 1 downto 0);
    signal overflow: bit;
    signal less: bit_vector(size-1 downto 0);
    signal f_temp: bit_vector(size-1 downto 0);
    constant zero: bit_vector(size - 1 downto 0) := (others => '0');
    signal SLT: bit_vector(size-1 downto 0);
begin


    ULA_propagada: for i in size - 1 downto 0 generate
    
        first_alu_1bit_generate: if i = 0 generate
            alu_1bit_first: alu_1_bit port map(A(i), B(i), primeiroCarry, S, resultado(i), vetorCarry(i),less(i));
        end generate first_alu_1bit_generate;

        alu_1bit_generate: if i > 0 generate
            alu_1bit: alu_1_bit port map(A(i), B(i), vetorCarry(i - 1), S, resultado(i), vetorCarry(i),less(i));
        end generate alu_1bit_generate;

    end generate ULA_propagada;
 
    primeiroCarry <= '1' when (S = "0110" or S = "0111" or S = "1010" or S="1100") else '0';

    Co <= vetorCarry(size-1);

    overflow <= vetorCarry(size-1) xor vetorCarry(size-2);

    Ov <= overflow;

    Z <= '1' when f_temp = zero else '0';

    SLT <= bit_vector(to_signed(1, size)) when ((overflow = '0' and resultado(size - 1) = '1') or (overflow = '1' and A(size-1) = '1' and B(size-1) = '0')) else bit_vector(to_signed(0, size));

    f_temp <=SLT when S = "0111" else resultado;

    F <= f_temp;
   
end architecture;

--REGFILE
library ieee;
use ieee.numeric_bit.all;
use ieee.math_real.all;
entity regfile is
    generic (
        reg_n : natural := 10;
        word_s : natural := 64
    );
    port (
        clock : in bit;
        reset : in bit;
        regWrite : in bit;
        rr1, rr2, wr: in bit_vector (natural (ceil(log2(real(reg_n)))) - 1 downto 0);
        d: in bit_vector(word_s-1 downto 0);
        q1, q2: out bit_vector(word_s-1 downto 0)
    );
end regfile;


architecture RegArch of regfile is
    type tipoRegVetor is array (0 to reg_n - 1) of bit_vector(word_s - 1 downto 0);
    signal RegVetor: tipoRegVetor;
    constant zero : bit_vector(word_s - 1 downto 0) := (others => '0');
begin

    process(clock, reset)
    begin
        if reset = '1' then
            RegVetor <= (others => zero);
        elsif rising_edge(clock) and regWrite = '1' and to_integer(unsigned(wr)) /= reg_n-1 then
            RegVetor(to_integer(unsigned(wr))) <= d;
        end if;
    end process;

    q1 <= RegVetor(to_integer(unsigned(rr1))) when to_integer(unsigned(rr1)) /= reg_n-1 else zero;
    q2 <= RegVetor(to_integer(unsigned(rr2))) when to_integer(unsigned(rr2)) /=  reg_n-1 else zero;
end architecture RegArch;

--SHIFTLEFT2
library ieee;
use ieee.numeric_bit.all;

entity shiftleft2 is
    generic(
        ws: natural := 64 -- word size
    );
    port (
        i: in bit_vector(ws-1 downto 0); -- input
        o: out bit_vector(ws-1 downto 0) -- output
    );
end shiftleft2;

architecture shifter of shiftleft2 is
    begin
        o <= i(ws-3 downto 0) & "00";
end architecture; -- arch


--SIGNEXTEND
library ieee;
use ieee.numeric_bit.all;

entity signExtend is 
    port(
        i: in bit_vector(31 downto 0); -- input
        o: out bit_vector(63 downto 0) -- output
    );
end signExtend;

architecture Extend of signExtend is
    begin
        o <= "000000000000000000000000000000000000000000000" & i(23 downto 5)
            when i(31 downto 24) = "10110100" and i(23) = '0' else
         "111111111111111111111111111111111111111111111" & i(23 downto 5)
            when i(31 downto 24) = "10110100" and i(23) = '1' else --CB

            "00000000000000000000000000000000000000" & i(25 downto 0)
            when i(31 downto 26) = "000101" and i(25) = '0' else
            "11111111111111111111111111111111111111" & i(25 downto 0)
            when i(31 downto 26) = "000101" and i(25) = '1' else --B

            "0000000000000000000000000000000000000000000000000000000" & i(20 downto 12)
            when  (i(31 downto 21) = "11111000000" or i(31 downto 21) ="11111000010") and i(20) = '0' else
            "1111111111111111111111111111111111111111111111111111111" & i(20 downto 12)
            when  (i(31 downto 21) = "11111000000" or i(31 downto 21) ="11111000010") and i(20) = '1' else --D

            (others => '0');
        
end architecture; -- arch

--REG
library IEEE;
use ieee.numeric_bit.all;
use ieee.math_real.ceil;
use ieee.math_real.log2;

entity reg is
    generic(
        wordSize: natural := 64
    );
    port(
        clock: in bit;
        reset: in bit;
        load: in bit;
        d: in bit_vector(wordSize-1 downto 0);
        q: out bit_vector(wordSize-1 downto 0)
    );
end reg;

architecture reg_arc of reg is
    signal d_int: bit_vector(wordSize-1 downto 0);
    begin
        process (clock, reset, load) 
        begin
            if (reset = '1') then
                d_int <= (others => '0');
            elsif (rising_edge(clock) and load = '1') then
                d_int <= d;
            end if;
        end process;
        q <= d_int;
end architecture reg_arc;



--UC
library IEEE;
use IEEE.numeric_bit.all;
entity controlunit is
    port (
    -- To Datapath
    reg2loc: out bit;
    uncondBranch: out bit;
    branch: out bit;
    memRead: out bit;
    memToReg: out bit;
    aluOp: out bit_vector(1 downto 0);
    memWrite: out bit;
    aluSrc: out bit;
    regWrite: out bit;
    --From Datapath
    opcode: in bit_vector(10 downto 0)
  ) ;
  end entity;

  architecture controlunit_arch of controlunit is
    begin 
   
    reg2loc <= '0' when (opcode="10001011000" or opcode ="11001011000" or opcode="10001010000" or opcode="10101010000") else '1';
    
    uncondBranch <= '1' when opcode(10 downto 5) = "000101" else '0';

    branch <='1' when opcode(10 downto 3) = "10110100" else '0';

    memRead <= '1' when opcode = "11111000010" else '0'; 

    memToReg <='0' when (opcode="10001011000" or opcode ="11001011000" or opcode="10001010000" or opcode="10101010000") else '1';

    aluOp <="00" when (opcode = "11111000010" or opcode = "11111000000") else "01" when opcode(10 downto 3) = "10110100" else "10";

    memWrite <='1' when opcode = "11111000000" else '0';

    aluSrc <='1' when opcode = "11111000010" or opcode = "11111000000" else '0';

    regWrite <='1' when ((opcode="10001011000" or opcode ="11001011000" or opcode="10001010000" or opcode="10101010000") or opcode="11111000010") else '0';


  end architecture;





--DATAPATH

library IEEE;
use IEEE.numeric_bit.all;
use IEEE.math_real.all;

entity datapath is 
    port (
        -- Common
        clock: in bit;
        reset: in bit;
        -- From Control Unit
        reg2loc: in bit;
        pcsrc: in bit;
        memToReg: in bit;
        aluCtrl: in bit_vector(3 downto 0);
        aluSrc: in bit;
        regWrite: in bit;
        -- To Control Unit
        opcode: out bit_vector(10 downto 0);
        zero: out bit;
        -- IM interface
        imAddr: out bit_vector(63 downto 0);
        imOut: in bit_vector(31 downto 0);
        -- DM interface
        dmAddr: out bit_vector(63 downto 0);
        dmIn: out bit_vector(63 downto 0);
        dmOut: in bit_vector(63 downto 0)
    );
end entity datapath;

architecture datapath_arch of datapath is
  component alu is
    generic (
        size: natural := 64
     );
     port (
      A,B : in bit_vector(size-1 downto 0); -- NUMEROS
      F: out bit_vector(size-1 downto 0); -- SAIDA
      S: in bit_vector(3 downto 0); -- SELECAO 
      Z: out bit; -- ZERO FLAG
      Ov: out bit; -- OVERFLOW
      Co: out bit -- CARRY OUT
     );
     end component;

     component reg is 
        generic(
             wordSize: natural := 64
        );
         port(
            clock: in bit;
            reset: in bit;
            load: in bit;
            d: in bit_vector(wordSize-1 downto 0);
            q: out bit_vector(wordSize-1 downto 0)
        );
    end component;

     component regfile is
      generic (
        reg_n : natural := 32;
        word_s : natural := 64
    );
    port (
      clock : in bit;
      reset : in bit;
      regWrite : in bit;
      rr1, rr2, wr: in bit_vector (natural (ceil(log2(real(reg_n)))) - 1 downto 0);
      d: in bit_vector(word_s-1 downto 0);
      q1, q2: out bit_vector(word_s-1 downto 0)
    );
    end component;

    component shiftleft2 is
      generic(
        ws: natural := 64 -- word size
    );
    port (
        i: in bit_vector(ws-1 downto 0); -- input
        o: out bit_vector(ws-1 downto 0) -- output
    );
    end component;

    component signExtend is
      port(
        i: in bit_vector(31 downto 0); -- INPUT
        o: out bit_vector(63 downto 0) -- OUTPUT
    );
    end component;
    signal Ov,Co: bit;
    signal saidaPC: bit_vector(63 downto 0);
    signal rr1, rr2, wr: bit_vector(4 downto 0); -- read register
    signal SignExtended,shifted: bit_vector(63 downto 0); --Sinal extendido, shifted left 2 e PC
    signal WD,readData1,readData2,A,B,F: bit_vector(63 downto 0); --write data,read data 1 e 2, A,B e resultado
    signal AddrPlusFour, AddrFinal, AddrJump: bit_vector(63 downto 0); --Endereços
    
    begin
        PC: reg Generic Map (
            wordSize => 64
        )
        port Map(
            clock => clock,
            reset => reset,
            load => '1',
            d => AddrFinal,
            q => saidaPC
        );

    wr <= imOut(4 downto 0);
    rr1 <= imOut(9 downto 5);
    rr2 <= imOut(20 downto 16) when reg2loc = '0' else imOut(4 downto 0);
    WD <= F when memToReg = '0' else dmOut;
          
    RegisterData: regfile port map (clock, reset, regWrite, rr1, rr2, wr, WD, readData1, readData2);
    
    A <=readData1;
    
    B<= readData2 when aluSrc = '0' else SignExtended;
    

    ULA: alu generic map (size => 64) port map (A, B, F, aluCtrl, zero, Ov,Co);


    SignExtender: signExtend port map (imOut, SignExtended);
    
    

    Shifter: shiftLeft2 Generic Map (
            ws => 64
        )
        port Map(
            i => SignExtended,
            o => shifted
        );

    opcode <= imOut(31 downto 21);

    
    imAddr <= saidaPC;
    dmIn <= readData2;  
    dmAddr <= F;

    ULA4: alu Generic map (
        size => 64
    )
    port Map(
        A => saidaPC,
        B => (2 => '1', others => '0'),
        F => AddrPlusFour,
        S => "0010",
        Z => open,
        Ov => open,
        Co => open
    );

        ULAJUMP: alu Generic map (
            size => 64
        )
        port Map(
            A => saidaPC,
            B => shifted,
            F => AddrJump,
            S => "0010",
            Z => open,
            Ov => open,
            Co => open
        );

    AddrFinal <= AddrPlusFour when pcsrc = '0' else AddrJump;
   

   
end architecture;



--POLILEG

library IEEE;
use IEEE.numeric_bit.all;
entity polilegsc is
    port (
        clock, reset: in bit;
        --Data Memory
        dmem_addr: out bit_vector(63 downto 0);
        dmem_dati: out bit_vector(63 downto 0);
        dmem_dato: in bit_vector(63 downto 0);
        dmem_we:   out bit;
        -- Instruction Memory
        imem_addr: out bit_vector(63 downto 0);
        imem_data: in bit_vector(31 downto 0)
    );
end entity;
architecture leg_arch of polilegsc is

  component  controlunit is
    port (
    -- To Datapath
    reg2loc: out bit;
    uncondBranch: out bit;
    branch: out bit;
    memRead: out bit;
    memToReg: out bit;
    aluOp: out bit_vector(1 downto 0);
    memWrite: out bit;
    aluSrc: out bit;
    regWrite: out bit;
    --From Datapath
    opcode: in bit_vector(10 downto 0)
  );
  end component;

  component datapath is 
port (
    --Common
    clock: in bit;
    reset: in bit;
    -- From Control Unit
    reg2loc: in bit;
    pcsrc: in bit;
    memToReg: in bit;
    aluCtrl: in bit_vector(3 downto 0);
    aluSrc: in bit;
    regWrite: in bit;
    -- To Control Unit
    opcode: out bit_vector(10 downto 0);
    zero: out bit;
    -- IM interface
    imAddr: out bit_vector(63 downto 0);
    imOut: in bit_vector(31 downto 0);
    -- DM interface
    dmAddr: out bit_vector(63 downto 0);
    dmIn: out bit_vector(63 downto 0);
    dmOut: in bit_vector(63 downto 0)
  );
  end component;

  signal reg2loc, pcsrc, memToReg, aluSrc, regWrite, zero, uncondBranch, branch, memRead: bit;
  signal opcode: bit_vector(10 downto 0);
  signal aluOp: bit_vector(1 downto 0);
  signal aluCtrl: bit_vector(3 downto 0);

  begin
    aluCtrl <= "0010" when (aluop = "00") else
                "0111" when (aluop = "01") else
                "0010" when (aluop = "10" and opcode = "10001011000") else
                "0110" when (aluop = "10" and opcode = "11001011000") else
                "0000" when (aluop = "10" and opcode = "10001010000") else
                "0001" when (aluop = "10" and opcode = "10101010000") else
                "0000";
    UC: controlunit port map (reg2loc, uncondBranch, branch, memRead, memToReg, aluOp, dmem_we, aluSrc, regWrite, opcode);
    DF: datapath port map (clock, reset, reg2loc, pcsrc, memToReg, aluCtrl, aluSrc, regWrite, opcode, zero, imem_addr, imem_data, dmem_addr, dmem_dati, dmem_dato);
    pcsrc <= (uncondBranch or (branch and zero));
    
    end architecture;