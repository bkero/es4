/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is [Open Source Virtual Machine.].
 *
 * The Initial Developer of the Original Code is
 * Adobe System Incorporated.
 * Portions created by the Initial Developer are Copyright (C) 2004-2006
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Adobe AS3 Team
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

package es4 
{
    /************************************************************************
     * Emitter for various data in ABC-compatible formats, as a byte stream.
     *
     * FIXME: this uses ActionScript's ByteArray, which is not at all
     * similar to the ByteArray in ES4.
     */

    import flash.utils.ByteArray;

    final class ABCByteStream 
    {
        function ABCByteStream() {
            bytes.endian = "littleEndian";
        }

        function get length() {
            return bytes.length;
        }

        function uint8(val:uint) {
            assert(val < 0x100);
            bytes.writeByte(val);
        }
        
        function uint16(val:uint) {
            assert(val < 0x10000);
            bytes.writeByte(val & 0xFF);
            bytes.writeByte((val >> 8) & 0xFF);
        }

        function int16(val:int) {
            assert(-0x8000 <= val && val < 0x8000);
            bytes.writeByte(val & 0xFF);
            bytes.writeByte((val >> 8) & 0xFF);
        }
        
        function int24(val:int) {
            assert(-0x1000000 <= val && val < 0x1000000);
            bytes.writeByte(val & 0xFF);
            bytes.writeByte((val >> 8) & 0xFF);
            bytes.writeByte((val >> 16) & 0xFF);
        }
        
        function uint30(val:uint) {
            assert(val < 0x40000000);
            uint32(val);
        }

        function int30(val:int) {
            assert(-0x40000000 <= val && val < 0x40000000);
            if (val < 0)
                uint32(-val);
            else
                uint32(uint(val));
        }

        function int32(val:int) {
            uint32(uint(val));
        }

        function uint32(val:uint) {
            if( val < 0x80 )               // 7 bits
                bytes.writeByte(val & 0x7F);
            else if ( val < 0x4000 ) {     // 14 bits
                bytes.writeByte((val & 0x7F) | 0x80);
                bytes.writeByte((val >> 7) & 0x7F);
            }
            else if ( val < 0x200000 ) {   // 21 bits
                bytes.writeByte((val & 0x7F) | 0x80);
                bytes.writeByte(((val >> 7) & 0x7F) | 0x80);
                bytes.writeByte((val >> 14) & 0x7F);
            }
            else if ( val < 0x10000000 ) { // 28 bits
                bytes.writeByte((val & 0x7F) | 0x80);
                bytes.writeByte(((val >> 7) & 0x7F) | 0x80);
                bytes.writeByte(((val >> 14) & 0x7F) | 0x80);
                bytes.writeByte((val >> 21) & 0x7F);
            }
            else {                         // 32 bits
                bytes.writeByte((val & 0x7F) | 0x80);
                bytes.writeByte(((val >> 7) & 0x7F) | 0x80);
                bytes.writeByte(((val >> 14) & 0x7F) | 0x80);
                bytes.writeByte(((val >> 21) & 0x7F) | 0x80);
                bytes.writeByte((val >> 28) & 0x7F);
            }
        }
        
        function float64(val:Number /*FIXME ES4: double*/) {
            bytes.writeDouble(val);
        }

        function utf8(str:String /*FIXME ES4: string*/) {
            bytes.writeUTFBytes(str);
        }

        function setInt24(loc, val) {
            assert(-0x1000000 <= val && val < 0x1000000);
            bytes.position = loc;
            bytes.writeByte(val & 0xFF);
            bytes.writeByte((val >> 8) & 0xFF);
            bytes.writeByte((val >> 16) & 0xFF);
            bytes.position = bytes.length;
        }

        function serialize(to:ABCByteStream) {
            to.byteStream(this);
        }

        function byteStream(from:ABCByteStream) {
            bytes.writeBytes(from.bytes);
        }

        function writeToArray(a) {
            bytes.position = 0;
            while ( bytes.bytesAvailable > 0 ) {
                var b = bytes.readByte();
                if (b < 0)
                    b += 256;
                a.push(b);
            }
            return a;
        }

        /* Returns *some* concrete ByteArray type, but the concrete
         * type is not part of the API here.  Clients must be adapted
         * to particular environments anyway.
         */
        function getBytes(): * { 
            return bytes; 
        }

        private const bytes = new flash.utils.ByteArray;
    }

    public function testABCByteStream() {
        print("--------------------------------------------");
        print("Testing ABCByteStream");
        print("");
            
        var bytes = new ABCByteStream;
        
        bytes.uint8(0x0A);
        bytes.uint16(0x010B);
        bytes.int16(-2);
        bytes.int24(257);
        bytes.uint30(7);
        bytes.int30(128);
        bytes.int30(-128);
        bytes.int32(10);
        bytes.uint32(0x0fffabcd);
        bytes.float64(1.0);
        bytes.utf8("foo");
        var b2 = new ABCByteStream;

        b2.uint8(0x0A);
        b2.uint8(0xFA);
        bytes.byteStream(b2);

        var result = "0A 0B 01 FE FF 01 01 00 07 80 01 80 01 0A CD D7 FE 7F 00 00 00 00 00 00 FF 3F 66 6F 6F 0A FA".split(" ");
        dumpByteStream( bytes, result );
    }
}
