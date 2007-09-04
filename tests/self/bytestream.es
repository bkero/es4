/* -*- mode: java; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- */
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

{
    /* Emitter for various data in ABC-compatible formats, as a byte
     * stream.  The byte stream is represented as an array of unsigned
     * integers below 256 here, the purpose is that we will dump its
     * contents to an external medium and load it into a VM.
     *
     * All data are dumped in litte-endian format.
     */

    class ABCByteStream
    {
        function get length() {
            return bytes.length;
        }

        function uint8(val:uint) {
            Util::assert(val < 256);
            bytes.push(val);
        }

        function uint16(val:uint) {
            Util::assert(val < 65536);
            bytes.push(val & 0xFF,
                       (val >> 8) & 0xFF);
        }

        function int16(val:int) {
            Util::assert(-32768 <= val && val < 32768);
            bytes.push(val & 0xFF,
                       (val >> 8) & 0xFF);
        }

        function int24(val:int) {
            Util::assert(-16777216 <= val && val < 16777216);
            bytes.push(val & 0xFF,
                       (val >> 8) & 0xFF,
                       (val >> 16) & 0xFF);
        }

        function uint30(val:uint) {
            Util::assert(val < 1073741824);
            uint32(val);
        }

        function int30(val:int) {
            Util::assert(-1073741824 <= val && val < 1073741824);
            if (val < 0)
                uint32(-val);
            else
                uint32(toUint(val));
        }

        function int32(val:int) {
            uint32(toUint(val));
        }

        function uint32(val:uint) {
            if( val < 0x80 )
                // 7 bits
                bytes.push(val & 0x7F);
            else if ( val < 0x4000 )
                // 14 bits
                bytes.push((val & 0x7F) | 0x80,
                           (val >> 7) & 0x7F);
            else if ( val < 0x200000 )
                // 21 bits
                bytes.push((val & 0x7F) | 0x80,
                           ((val >> 7) & 0x7F) | 0x80,
                           (val >> 14) & 0x7F);
            else if ( val < 0x10000000 )
                // 28 bits
                bytes.push((val & 0x7F) | 0x80,
                           ((val >> 7) & 0x7F) | 0x80,
                           ((val >> 14) & 0x7F) | 0x80,
                           (val >> 21) & 0x7F);
            else
                // 32 bits
                bytes.push((val & 0x7F) | 0x80,
                           ((val >> 7) & 0x7F) | 0x80,
                           ((val >> 14) & 0x7F) | 0x80,
                           ((val >> 21) & 0x7F) | 0x80,
                           (val >> 28) & 0x7F);
        }

        function float64(val /*FIXME ES4: double*/) {
            var bs = explodeNumber(val);  /*FIXME ES4: destructuring*/
            bytes.push(bs[0], bs[1], bs[2], bs[3], bs[4], bs[5], bs[6], bs[7]);
        }

        function utf8(str /*FIXME ES4: string*/) {
            for ( var i=0, limit=str.length ; i < limit ; i++ ) {
                var c = str.charCodeAt(i);
                if (c <= 0x7F)
                    bytes.push(c);
                else if (c <= 0x7FF)
                    bytes.push(0xC0 | ((c >> 6) & 0x1F),
                               0x80 | (c & 0x3F));
                else if (c <= 0xFFFF)
                    bytes.push(0xD0 | ((c >> 12) & 0x0F),
                               0x80 | ((c >> 6) & 0x3F),
                               0x80 | (c & 0x3F));
                else
                    bytes.push(0xF0 | ((c >> 18) & 0x07),
                               0x80 | ((c >> 12) & 0x3F),
                               0x80 | ((c >> 6) & 0x3F),
                               0x80 | (c & 0x3F));
            }
        }

        function setInt24(loc, val) {
            Util::assert(-16777216 <= val && val < 16777216);
            bytes[loc] = val & 0xFF;
            bytes[loc+1] = (val >> 8) & 0xFF;
            bytes[loc+2] = (val >> 16) & 0xFF;
        }

        function serialize(s:ABCByteStream) {
            s.byteStream(this);
        }

        function byteStream(from:ABCByteStream) {
            var from_bytes = from.bytes;
            for ( var i=0, limit=from_bytes.length ; i < limit ; i++ )
                bytes.push(from_bytes[i]);
        }

        function writeToArray(a) {
            return copyArray(bytes);
        }

        /* Returns *some* concrete byte-array type, but the concrete
         * type is not part of the API here.  Clients must be adapted
         * to particular environments anyway.
         */
        function getBytes(): * {
            return bytes;
        }

        /*private*/ const bytes = [];
    }
}
