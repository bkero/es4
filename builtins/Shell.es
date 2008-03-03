/*
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
 *
 */

package
{
    // non-standard helpers that a variety of tests in our testsuite use.

    intrinsic native function print(x);
    intrinsic native function load(x);
    intrinsic native function readFile(x);
    intrinsic native function readHTTP(server, port, file, method, headers);
    intrinsic function readURL(url, method, headers) {
        function eatLine(str, i) {
            for (let j = i; j < str.length; j++) {
                if (str[j] === '\n') {
                    return [str.substring(i,j+1).trim(), j+1];
                }
            }
            return [str.substring(i), str.length];
        }
        function eatHeaders(str, i) {
            let result = {};
            let line, j;
            for ([line, j] = eatLine(str, i); j < str.length; [line, j] = eatLine(str, j)) {
                if (line.trim() === "") {
                    break;
                }
                let m = line.match(/(^[^:]+):(.*)/);
                if (m === null) {
                    throw ("couldn't parse HTTP header: " + line);
                }
                result[m[1].trim()] = m[2].trim();
            }
            return [result, j];
        }
        function parseStatus(text) {
            let m = text.match(/^[^ ]+[ \t]+(\d+)[ \t]+(.*)/);
            if (m === null) {
                throw ("couldn't parse HTTP status: " + text);
            }
            return [uint(m[1].trim()), m[2].trim()];
        }
        function parseResponse(rsp) {
            let [status, i] = eatLine(rsp,0);
            let [headers, j] = eatHeaders(rsp,i);
            let [statusCode, statusText] = parseStatus(status);
            return ({ statusText: statusText,
                      status: statusCode,
                      headers: headers,
                      responseText: rsp.substring(j) });
        }
        url = String(url);
        method = String(method || "GET");
        headers = headers || {};
        let headerStr = "";
        for (let key in headers) {
            headerStr += String(key) + ": " + String(headers[key]) + "\r\n";
        }
        let matches = url.match(/(^http:\/\/)([^\/:]+)(?::(\d+))?(\/.*)?/);
        if (matches === null) {
            throw ("couldn't parse URL: " + url);
        }
        let server = String(matches[2]);
        let port = uint(matches[3] || 80);
        let page = String(matches[4] || "/");
        return parseResponse(intrinsic::readHTTP(server, port, page, method, headerStr));
    }
    intrinsic native function assert(x);
    intrinsic native function typename(x);
    intrinsic native function inspect(x, depth);
    intrinsic native function dumpFunc(x:Function);
    intrinsic native function proto(x);
    intrinsic native function id(x);

    public const print = intrinsic::print;
    public var gVersion = 0;
    public function version(v) {
        if (v) {
            gVersion = v;
        }
        return gVersion;
    }
    public function toString() { return "[object global]" }

}
