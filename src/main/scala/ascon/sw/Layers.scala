package ascon.sw

import scala.collection.concurrent.TrieMap


object Constant {

  def addConstant(a: BigInt, round: Int): BigInt = {
    val roundConst: Seq[BigInt] = Seq(BigInt("F0", 16), BigInt("E1", 16), BigInt("D2", 16),
      BigInt("C3", 16), BigInt("B4", 16), BigInt("A5", 16), BigInt("96", 16), BigInt("87", 16),
      BigInt("78", 16), BigInt("69", 16), BigInt("5a", 16), BigInt("4b", 16))

    val xorconst = a ^ roundConst(round)
    xorconst
  }

  def Layer(X: Array[BigInt], round: Int): Array[BigInt] = {
    val S = Array(X(0), X(1), addConstant(X(2), round), X(3), X(4))
    S
  }
}

object Difussion {
  def rightRotationBig(a: BigInt, b: Int, nibbles: Int): BigInt = {
    // The shift functions increase the length of the
    val mask = BigInt("F" * nibbles, 16)
    val lsb = a >> b
    val shift = nibbles * 4 - b
    val mask_shift = mask >> shift
    val msb = a & mask_shift
    val msb2 = msb << shift
    val rotate = lsb | msb2
    rotate
  }

  def asconDiffusion(row64: BigInt, shift1: Int, shift2: Int): BigInt = {
    val x0_1 = rightRotationBig(row64, shift1, 16)
    val x0_2 = rightRotationBig(row64, shift2, 16)
    val x0 = row64 ^ x0_1 ^ x0_2
    // "h" + x0.toString(16)
    x0
  }

  def Layer(X: Array[BigInt]): Array[BigInt] = {
    val S = Array(
      asconDiffusion(X(0), 19, 28),
      asconDiffusion(X(1), 61, 39),
      asconDiffusion(X(2), 1, 6),
      asconDiffusion(X(3), 10, 17),
      asconDiffusion(X(4), 7, 41))
    S
  }
}

object Sustitution {
  def notBigInt(a: BigInt): BigInt = {
    val mask = BigInt(("F" * 16), 16)
    mask &~ a
  }

  def Layer(X: Array[BigInt]): Array[BigInt] = {
    /**
     * página 33 figura 6 bitsliced 5-bit S-box S(x)
     * x0 += x4;     x4 +=  x3;     x2 += x1;
     * t0 = (~x4) & x0;
     * t1 = (~x1) & x2;     x0 += t1;
     * t1 = (~x3) & x4;     x2 += t1;
     * t1 = (~x0) & x1;     x4 += t1;
     * t1 = (~x2) & x3;     x1 += t1;
     * x3 += t0;
     * x1 += x0;     x3 +=  x2;     x0 += x4;     x2 += ~x2;
     */
    val x0 = X(0) ^ X(4);
    val x4 = X(4) ^ X(3);
    val x2 = X(2) ^ X(1);
    val t0 = x0 & notBigInt(x4);
    val t1 = x2 & notBigInt(X(1));
    val x00 = x0 ^ t1;
    val t11 = x4 & notBigInt(X(3));
    val x22 = x2 ^ t11;
    val t111 = X(1) & notBigInt(x00);
    val x44 = x4 ^ t111;
    val t1111 = X(3) & notBigInt(x22);
    val x1 = X(1) ^ t1111;
    val x3 = X(3) ^ t0;
    val s1 = x1 ^ x00;
    val s3 = x3 ^ x22;
    val s0 = x00 ^ x44;
    val s2 = notBigInt(x22);
    val s4 = x44;
    val S = Array(s0, s1, s2, s3, s4)
    S

  }
}

object cipherTextTag {
  val C = Array(BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0))
  val Hash = Array(BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0))
  val Dec = Array(BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0), BigInt(0))
  val Tag = Array(BigInt(0), BigInt(0))
  val Tag2 = Array(BigInt(0), BigInt(0))
}

object Paddconst {

  val andCte = Array(BigInt("0000000000000000", 16),
    BigInt("ff00000000000000", 16),
    BigInt("ffff000000000000", 16),
    BigInt("ffffff0000000000", 16),
    BigInt("ffffffff00000000", 16),
    BigInt("ffffffffff000000", 16),
    BigInt("ffffffffffff0000", 16),
    BigInt("ffffffffffffff00", 16),
    BigInt("ffffffffffffffff", 16))
  val xorCte = Array(BigInt("8000000000000000", 16),
    BigInt("0080000000000000", 16),
    BigInt("0000800000000000", 16),
    BigInt("0000008000000000", 16),
    BigInt("0000000080000000", 16),
    BigInt("0000000000800000", 16),
    BigInt("0000000000008000", 16),
    BigInt("0000000000000080", 16),
    BigInt("0000000000000000", 16))
  val andClear = Array(
    BigInt("ffffffffffffffff", 16),
    BigInt("00ffffffffffffff", 16),
    BigInt("0000ffffffffffff", 16),
    BigInt("000000ffffffffff", 16),
    BigInt("00000000ffffffff", 16),
    BigInt("0000000000ffffff", 16),
    BigInt("000000000000ffff", 16),
    BigInt("00000000000000ff", 16),
    BigInt("0000000000000000", 16))
}

object ASCON {
  def permutation(X: Array[BigInt], round: Int): Array[BigInt] = {
    val Const = Constant.Layer(X, round)
    val Sust = Sustitution.Layer(Const)
    val Diff = Difussion.Layer(Sust)
    Diff

  }

  def permutationa(S: Array[BigInt], rounds: Int): Array[BigInt] = {

    var a = 0;
    if (rounds == 8) {
      a = 4
    } else if (rounds == 6) {
      a = 6
    }
    var S_out = permutation(S, a)

    //println(s"a = $a")
    printState(S_out, s"a = $a")
    for (b <- a + 1 until 12) {
      //println(s"b = $b")
      S_out = permutation(S_out, b)
      printState(S_out, s"a = $b")
    }
    S_out
  }

  def initialization(S: Array[BigInt], key: Array[BigInt], nonce_as: Array[BigInt]): Array[BigInt] = {
    var S_out = S

    val IV = BigInt("80400c0600000000", 16)
    S_out(0) = IV
    S_out(1) = key(0)
    S_out(2) = key(1)
    S_out(3) = nonce_as(0)
    S_out(4) = nonce_as(1)

    var S_aux = permutationa(S_out, 12)
    S_out = S_aux
    S_out(3) = S_out(3) ^ key(0)
    S_out(4) = S_out(4) ^ key(1)
    S_out

  }

  def init_Hash(S: Array[BigInt], iv: BigInt): Array[BigInt] = {
    var S_out = S

    S_out(0) = iv
    S_out(1) = 0
    S_out(2) = 0
    S_out(3) = 0
    S_out(4) = 0

    val S_aux = permutationa(S_out, 12)
    S_aux

  }

  def finalization(S: Array[BigInt], key: Array[BigInt]): Array[BigInt] = {
    var S_out = S
    S_out(1) = S_out(1) ^ key(0)
    S_out(2) = S_out(2) ^ key(1)
    printState(S_out, "Fin + key")
    S_out = permutationa(S_out, 12)
    printState(S_out, mes = "Fin before last key")
    S_out(3) = S_out(3) ^ key(0)
    S_out(4) = S_out(4) ^ key(1)
    S_out

  }

  def pad(A: BigInt, mlen: Int): BigInt = {


    val andPad = Paddconst.andCte(mlen)
    val xorPad = Paddconst.xorCte(mlen)
    val Aand = A & andPad
    val Axor = Aand ^ xorPad

    Axor


  }

  def PadOnly1(mlen: Int): BigInt = {
    val xorPad = Paddconst.xorCte(mlen)
    xorPad
  }

  def trimM(M: BigInt, S: BigInt, mlen: Int): BigInt = {
    val andPad = Paddconst.andCte(mlen)
    val Aand = (S ^ M) & andPad
    Aand
  }

  def trimC(C: BigInt, clen: Int): BigInt = {
    val andPad = Paddconst.andCte(clen)
    val Ctrim = C & andPad
    Ctrim
  }

  def ClearBytes(C: BigInt, clen: Int): BigInt = {
    val andClear = Paddconst.andClear(clen)
    val res = C & andClear
    res
  }

  def printState(S: Array[BigInt], mes: String, debug_in: Boolean = false): Unit = {

    if (debug_in) {
      println(mes)
      println("S(0):  " + S(0).toString(16))
      println("S(1):  " + S(1).toString(16))
      println("S(2):  " + S(2).toString(16))
      println("S(3):  " + S(3).toString(16))
      println("S(4):  " + S(4).toString(16))
      println()
    }

  }

  def ascon_encription(m: Array[BigInt], mlen: Int, ad: Array[BigInt],c :Array[BigInt],
                       adlen: Int, npub: Array[BigInt], key: Array[BigInt]): Unit = {
    var State = Array(
      BigInt(0),
      BigInt(0),
      BigInt(0),
      BigInt(0),
      BigInt(0)
    )
    State = initialization(State, key, npub)
    printState(State, "After init")


    // Xor init

    printState(State, "Xor key 2")
    var adlen_aux = adlen
    var adindex = 0
    if (adlen_aux > 0) {
      while (adlen_aux >= 8) {
        State(0) ^= ad(adindex)
        State = permutationa(State, 6)
        printState(State, s"Absorb AD [$adindex]")

        adindex += 1
        adlen_aux -= 8
      }
      State(0) ^= pad(ad(adindex), adlen_aux)
      printState(State, "Paded data")
      State = permutationa(State, 6)
      printState(State, "Paded round data")
    }
    // Domain separation
    State(4) ^= BigInt("0000000000000001", 16)
    printState(State, "AD Domain Separartion")
    var mlen_aux = mlen
    var mindex = 0
    // Plain text processing
    while (mlen_aux >= 8) {
      State(0) ^= m(mindex)
      c(mindex) = State(0)
      printState(State, s"Absorb M [$mindex]")
      State = permutationa(State, 6)
      mindex += 1
      mlen_aux -= 8
    }
    // final block
    printState(State, "before MPad")
    //println(s"mlen_aux: $mlen_aux")
    c(mindex) = trimM(m(mindex), State(0), mlen_aux)
    //println(trimM(m(mindex), State(0), mlen_aux).toString(16))
    //println(s"mindex $mindex: " + cipherTextTag.C(mindex).toString(16))
    State(0) ^= pad(m(mindex), mlen_aux)
    //println("pad " + pad(m(mindex), mlen_aux).toString(16))

    printState(State, "pad Plain")

    State = finalization(State, key)
    printState(State, "fin")
    cipherTextTag.Tag(0) = State(3)
    cipherTextTag.Tag(1) = State(4)

  }

  def ascon_decription(c: Array[BigInt], clen: Int, ad: Array[BigInt], dec : Array[BigInt],
                       adlen: Int, npub: Array[BigInt], key: Array[BigInt]): Unit = {
    var State = Array(
      BigInt(0),
      BigInt(0),
      BigInt(0),
      BigInt(0),
      BigInt(0)
    )
    State = initialization(State, key, npub)
    printState(State, "After init")


    // Xor init

    printState(State, "Xor key 2")
    var adlen_aux = adlen
    var adindex = 0
    if (adlen_aux > 0) {
      while (adlen_aux >= 8) {
        State(0) ^= ad(adindex)
        State = permutationa(State, 6)
        printState(State, s"Absorb AD [$adindex]")

        adindex += 1
        adlen_aux -= 8
      }
      State(0) ^= pad(ad(adindex), adlen_aux)
      printState(State, "Paded data")
      State = permutationa(State, 6)
      printState(State, "Paded round data")
    }
    // Domain separation
    State(4) ^= BigInt("0000000000000001", 16)
    printState(State, "AD Domain Separartion")
    var clen_aux = clen
    var cindex = 0
    var c0 = BigInt("0000000000000000", 16);
    // Plain text processing
    while (clen_aux >= 8) {
      c0 = c(cindex)
      dec(cindex) = c0 ^ State(0)

      State(0) = c0
      printState(State, s"Absorb C [$cindex]")
      State = permutationa(State, 6)
      cindex += 1
      clen_aux -= 8
    }
    // final block
    printState(State, "before CPad")
    c0 = trimC(c(cindex), clen_aux)
    dec(cindex) = trimM(c(cindex), State(0), clen_aux)
    State(0) = ClearBytes(State(0), clen_aux)
    State(0) = State(0) | c0
    State(0) ^= PadOnly1(clen_aux)

    printState(State, "pad Plain")

    State = finalization(State, key)
    printState(State, "fin")
    cipherTextTag.Tag2(0) = State(3)
    cipherTextTag.Tag2(1) = State(4)

  }

  def ascon_Hash(m: Array[BigInt], mlen: Int, hash: Array[BigInt]): Unit = {
    var State = Array(
      BigInt(0),
      BigInt(0),
      BigInt(0),
      BigInt(0),
      BigInt(0)
    )
    val IV = BigInt("00400c0000000100", 16)
    State = init_Hash(State, IV)
    printState(State, "After init")

    var mlen_aux = mlen
    var mindex = 0
    if (mlen_aux > 0) {
      while (mlen_aux >= 8) {
        State(0) ^= m(mindex)
        printState(State, s"Absorb M [$mindex]")
        State = permutationa(State, 12)

        mindex += 1
        mlen_aux -= 8
      }
      State(0) ^= pad(m(mindex), mlen_aux)
      printState(State, "Paded data")
      State = permutationa(State, 12)
      printState(State, "Paded round data")
    }
    // Squeeze hash
    var i = 0
    while (i < 4) {
      hash(i) = State(0)
      printState(State, s"Extract Hash [$i]")
      if (i < 4 - 1) {
        State = permutationa(State, 12)
      }
      i += 1
    }


  }


}


object Hello extends App {

  //  var State2 = Array(
  //    BigInt(0),
  //    BigInt(0),
  //    BigInt(0),
  //    BigInt(0),
  //    BigInt(0)
  //  )
  //
  //  ASCON.printState(State2)
  //  val State_aux = ASCON.permutationa(State2,6)
  //  println("Hie")
  //  ASCON.printState(State_aux)

  var message = Array(
    BigInt("abcd012abf290cba", 16),
    BigInt("5678c02803a01165", 16),
    BigInt("5678518020108765", 16),
    BigInt("5678a010a0908765", 16)
  )

  var assodat = Array(
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0)
  )
  var hash = Array(
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0)
  )
  var c = Array(
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0)
  )

  var dec = Array(
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0),
    BigInt(0)
  )


  var Key = Array(
    BigInt("abcd00000000dcba", 16),
    BigInt("5678000000008765", 16)
  )
  var Numpub = Array(
    BigInt("1234000000004321", 16),
    BigInt("4321000000001234", 16)
  )

  val mlen = 21
  val adlen = 5
  val res2 = ASCON.ascon_encription(message,mlen,assodat,c,adlen,Numpub,Key)
  val res = ASCON.ascon_decription(c, mlen, assodat,dec, adlen, Numpub, Key)

  println(cipherTextTag.Tag(0).toString(16) + " " + cipherTextTag.Tag(1).toString(16))
  println()
  println(cipherTextTag.Tag2(0).toString(16) + " " + cipherTextTag.Tag2(1).toString(16))

//  val res2 = ASCON.ascon_Hash(message, mlen, hash)
//  println("HashResult")
  println(dec(0).toString(16))
  println(dec(1).toString(16))
  println(dec(2).toString(16))
  println(dec(3).toString(16))
  println(dec(4).toString(16))
  println(dec(5).toString(16))
//  println()


}
