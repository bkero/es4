(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 *
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 *
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 *
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * End of Terms and Conditions
 *
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *)
structure Fixture :> FIXTURE = struct

val doTrace = ref false
fun log ss = LogErr.log ("[fixture] " :: ss)
fun error ss = LogErr.fixtureError ss
fun trace ss = if (!doTrace) then log ss else ()

structure NmKey = struct type ord_key = Ast.NAME val compare = NameKey.compare end
structure NmMap = SplayMapFn (NmKey);

structure NmVecKey = struct type ord_key = (Ast.NAME vector) val compare = (Vector.collate NameKey.compare) end
structure NmVecMap = SplayMapFn (NmVecKey);

(* -----------------------------------------------------------------------------
 * Operations on FIXTURE
 * ----------------------------------------------------------------------------- *)

fun printFixture ((n:Ast.FIXTURE_NAME), (f:Ast.FIXTURE)) =
    let
	val fs = case f of
		     Ast.NamespaceFixture _ => "[namespace]"
		   | Ast.ClassFixture _ => "[class]"
		   | Ast.InterfaceFixture _ => "[interface]"
		   | Ast.TypeVarFixture => "[typeVar]"
		   | Ast.TypeFixture _ => "[type]"
		   | Ast.MethodFixture _ => "[method]"
		   | Ast.ValFixture _ => "[val]"
		   | Ast.VirtualValFixture _ => "[virtualVal]"
    in
	case n of
	    Ast.TempName n => trace ["temp #", Int.toString n, " -> ", fs]
      | Ast.PropName n => trace [LogErr.name n, " -> ", fs]
    end

(* -----------------------------------------------------------------------------
 * Operations on FIXTURES
 * ----------------------------------------------------------------------------- *)

fun getFixture (b:Ast.FIXTURES)
               (n:Ast.FIXTURE_NAME)
    : Ast.FIXTURE =
    let
        fun search [] = LogErr.hostError ["fixture binding not found: ",
                                          (LogErr.fname n)]
          | search ((k,v)::bs) =
            if k = n
            then v
            else search bs
    in
        search b
    end

fun hasFixture (b:Ast.FIXTURES)
               (n:Ast.FIXTURE_NAME)
    : bool =
    let
        fun search [] = false
          | search ((k,v)::bs) =
            if k = n
            then true
            else search bs
    in
        search b
    end

fun replaceFixture (b:Ast.FIXTURES)
                   (n:Ast.FIXTURE_NAME)
                   (v:Ast.FIXTURE)
    : Ast.FIXTURES =
    let
        fun search [] = LogErr.hostError ["fixture binding not found: ",
                                          (LogErr.fname n)]
          | search ((k,v0)::bs) =
            if k = n
            then (k,v)::bs
            else (k,v0) :: (search bs)
    in
        search b
    end

fun printFixtures fs =
    List.app printFixture fs

(* -----------------------------------------------------------------------------
 * Operations on TOP_FIXTURES
 * ----------------------------------------------------------------------------- *)

type TOP_FIXTURES = { fixtureCache: (Ast.FIXTURE NmMap.map) ref,
                      instanceOfCache: (bool NmVecMap.map) ref,
                      cacheSize: int,
                      all: Ast.FIXTURES };

fun mkTopFixtures (all:Ast.FIXTURES)
    : TOP_FIXTURES =
    { fixtureCache = ref NmMap.empty,
      instanceOfCache = ref NmVecMap.empty,
      cacheSize = 1024,
      all = all }

fun getTopFixture (tf:TOP_FIXTURES)
                  (n:Ast.NAME)
    : Ast.FIXTURE =
    let
        val { fixtureCache, cacheSize, all, ... } = tf
        val c = !fixtureCache
    in
        case NmMap.find (c, n) of
            NONE =>
            let
                val v = getFixture all (Ast.PropName n)
            in
                if (NmMap.numItems c) < cacheSize
                then (fixtureCache := NmMap.insert (c, n, v); v)
                else v
            end
          | SOME v => v
    end

val (getTopFixtures:TOP_FIXTURES -> Ast.FIXTURES) = #all

fun printTopFixtures (tf:TOP_FIXTURES) =
    printFixtures (getTopFixtures tf)

(* -----------------------------------------------------------------------------
 * Class operations on TOP_FIXTURES
 * ----------------------------------------------------------------------------- *)

(* FIXME: these really belong in type.sml but then I can't keep the
 *        instanceOfCache in TOP_FIXTURES if it's an abstract type
 *)

fun instanceType (tf:TOP_FIXTURES)
                 (n:Ast.NAME)
    : Ast.INSTANCE_TYPE =
    case getTopFixture tf n of
        Ast.ClassFixture (Ast.Cls cls) => (#instanceType cls)
      | Ast.InterfaceFixture (Ast.Iface iface) => (#instanceType iface)
      | _ => error ["type not an instance type ", LogErr.name n]

fun isClass (tf:TOP_FIXTURES)
            (t:Ast.NAME)
    : bool =
    case getTopFixture tf t of
        Ast.ClassFixture cls => true
      | _ => false

fun getClass (tf:TOP_FIXTURES)
             (t:Ast.NAME)
    : Ast.CLS =
    case getTopFixture tf t of
        Ast.ClassFixture cls => cls
      | Ast.InterfaceFixture iface =>   (* FIXME: not sure what to do here. getClass gets called when a nominal
                                            type is used in various ways. Just return class Object for now *)
        let
            val Ast.ClassFixture objCls = getTopFixture tf Name.nons_Object
        in
            objCls
        end
      | _ => error ["getClass returned non-class fixture for ", LogErr.name t]

(* FIXME: this is a bad name. subclassOf isn't quite right either. isDerivedType? inheritsFrom? *)
fun instanceOf (tf:TOP_FIXTURES)
               (t0:Ast.NAME)
               (t:Ast.NAME)
    : bool =
    let
        val { instanceOfCache, cacheSize, ... } = tf
        val c = !instanceOfCache
        fun search n =
            if Mach.nameEq n t
            then true
            else
                let
                    val it = instanceType tf n
                    val bases = (#superTypes it)
                in
                    List.exists search bases
                end
        val k = Vector.fromList [t0,t]
    in
        case NmVecMap.find (c, k) of
            NONE =>
            let
                val v = search t0
            in
                if (NmVecMap.numItems c) < cacheSize
                then (instanceOfCache := NmVecMap.insert (c, k, v); v)
                else v
            end
          | SOME v => v
    end

end
