/* -*- mode: java; indent-tabs-mode: nil -*-
 *
 * ECMAScript 4 builtins - the "date" object
 *
 * E262-3 15.9
 * E262-4 proposals:date_and_time
 *
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
 *
 * Status: complete; not reviewed; not tested.
 *
 * The Emacs java formatting mode often fails here because of the
 * extensive use of expression functions.
 */

    use namespace ECMAScript4_Internal;
    use namespace helper;
    use namespace informative;
    use namespace intrinsic;

    use default namespace public;

    const NOARG = {};

    dynamic class Date
    {

        static const length = 7;

        /* E262-3 15.9.2: The Date Constructor Called as a Function */
        meta static function invoke(...args)   // args are ignored.
            (new Date()).public::toString();

        /* E262-3 15.9.3: The Date Constructor. */
        function Date(year=NOARG, month=NOARG, date=NOARG, hours=NOARG, minutes=NOARG, seconds=NOARG, ms=NOARG) {
            informative::setupNanoAge();

            switch (NOARG) {
            case year:
                timeval = Date.now();
                return;

            case month: {
                let v = ToPrimitive(year);
                if (v is string)
                    return parse(v);

                timeval = TimeClip(double(v));
                return;
            }

            default:
                ms = double(ms);

            case ms:
                seconds = double(seconds);

            case seconds:
                minutes = double(minutes);

            case minutes:
                hours = double(hours);

            case hours:
                date = double(date);

            case date:
                year = double(year);
                month = double(month);

                let intYear : double = helper::toInteger(year);
                if (!isNaN(year) && 0 <= intYear && intYear <= 99)
                    intYear += 1900;
                timeval = TimeClip(UTCTime(MakeDate(MakeDay(intYear, month, date),
                                                    MakeTime(hours, minutes, seconds, ms))));
            }
        }

        /* E262-3 15.9.4.2: Date.parse
           E262-4 proposals:date_and_time - "ISO Date strings".

           Note incompatibilities with ES3, which allowed arbitrary
           extra arguments to Date.parse and ignored them all
           (standard behavior): E262-3 did not include a clause about
           maybe interpreting extra arguments differently in the
           future for Date.parse, unlike for some other methods.  This
           shows that that kind of imprecation either (a) should be
           the default for all library methods or (b) is pointless.
           Take your pick.
        */
        static var parse = function parse(str, reference:double=0.0) {
            return Date.parse(string(str), reference);
        }

        static intrinsic function parse(s:string, reference:double=0.0) : double {

            function fractionToMilliseconds(frac: string): double
                Math.floor(1000 * (parseInt(frac) / Math.pow(10, frac.length)));

            let isoRes = isoTimestamp.exec(s);
            let defaults = new Date(reference);
            if (isoRes) {
                let year = isoRes.year !== undefined ? parseInt(isoRes.year) : defaults.UTCYear;
                let month = isoRes.month !== undefined ? parseInt(isoRes.month)-1 : defaults.UTCMonth;
                let day = isoRes.day !== undefined ? parseInt(isoRes.day) : defaults.UTCDay;
                let hour = isoRes.hour !== undefined ? parseInt(isoRes.hour) : defaults.UTCHour;
                let mins = isoRes.minutes !== undefined ? parseInt(isoRes.minutes) : defaults.UTCMinutes;
                let secs = isoRes.seconds !== undefined ? parseInt(isoRes.seconds) : defaults.UTCSeconds;
                let millisecs = isoRes.fraction !== undefined ?
                    fractionToMilliseconds(isoRes.fraction) :
                    defaults.UTCMilliseconds;
                let tzo = defaults.timezoneOffset;
                if (isoRes.zulu !== undefined)
                    tzo = 0;
                else if (isoRes.offs !== undefined) {
                    tzo = parseInt(isoRes.tzhr) * 60;
                    if (isoRes.tzmin !== undefined)
                        tzo += parseInt(isoRes.tzmin);
                    if (isoRes.tzdir === "-")
                        tzo = -tzo;
                }
                return new Date.UTC(year, month, day, hour, mins, secs, millisecs) - tzo;
            }
            else
                return informative::fromDateString(s, reference);
        }

        /* E262-4 proposals:date_and_time - "Current and elapsed times" */
        static intrinsic native function now() : double;

        static function now() : double
            Date.intrinsic::now();

        /* E262-4 proposals:date_and_time - "Current and elapsed times" */
        prototype function nanoAge() : double
            this.nanoAge();

        intrinsic function nanoAge() : double
            informative::nanoAge();

        /* INFORMATIVE */
        informative function nanoAge() : double
            (Date.now() - birthtime) * 1000000;

        /* INFORMATIVE */
        informative function setupNanoAge() : void
            this.birthtime = Date.now();

        /* E262-3 15.9.4.3: Date.UTC */
        static var UTC =
            function UTC(year, month, date=NOARG, hours=NOARG, minutes=NOARG, seconds=NOARG, ms=NOARG) {
                switch (NOARG) {
                case date:    date = 1;
                case hours:   hours = 0;
                case minutes: minutes = 0;
                case seconds: seconds = 0;
                case ms:      ms = 0;
                }
                return Date.UTC(double(year),
                                double(month),
                                double(date),
                                double(hours),
                                double(minutes),
                                double(seconds),
                                double(ms));
            };

        static intrinsic function UTC(year: double, 
                                      month: double,
                                      date: double=1, 
                                      hours: double=0, 
                                      minutes: double=0,
                                      seconds: double=0, 
                                      ms: double=0) : double
        {
            let intYear = helper::toInteger(year);
            if (!isNaN(year) && 0 <= intYear && intYear <= 99)
                intYear += 1900;
            return TimeClip(MakeDate(MakeDay(intYear, month, date),
                                     MakeTime(hours, minutes, seconds, ms)));
        }

        /* E262-4 proposals:date_and_time - "ISO Date strings" */
        prototype function toISOString(this:Date)
            this.toISOString();

        intrinsic function toISOString() : string {
            return (formatYears(UTCFullYear) + "-" + 
                    zeroFill(UTCMonth+1, 2) + "-" + 
                    zeroFill(UTCDate, 2) +
                    "T" + 
                    zeroFill(UTCHours, 2) + ":" + 
                    zeroFill(UTCMinutes, 2) + ":" + 
                    zeroFill(UTCSeconds, 2) + "." + 
                    removeTrailingZeroes(int(UTCMilliseconds)) + 
                    "Z");
        }

        prototype function toJSONString(this:Date, pretty=false) 
            this.intrinsic::toJSONString(pretty);

        override intrinsic function toJSONString(pretty: boolean=false)
            JSON.formatDate(this, pretty);

        /* E262-3 15.9.5.2: Date.prototype.toString */
        prototype function toString(this:Date)
            this.intrinsic::toString();

        /* INFORMATIVE */
        override intrinsic function toString() : string {
            /* "Fri, 15 Dec 2006 23:45:09 GMT-0800" */
            let tz:double = timezoneOffset;
            let atz:double = Math.abs(tz);
            return (dayNames[day] + ", " +
                    twoDigit(date) + " " +
                    monthNames[month] + " " +
                    fullYear + " " +
                    twoDigit(hours) + ":" +
                    twoDigit(minutes) + ":" +
                    twoDigit(seconds) + " GMT" +
                    signString(tz) + twoDigit(Math.floor(atz / 60)) + twoDigit(atz % 60));
        }

        prototype function toGMTString(this:Date)
            this.toUTCString();

        /* E262-3 15.9.5.42: Date.prototype.toUTCString */
        prototype function toUTCString(this:Date)
            this.toUTCString();

        /* INFORMATIVE */
        intrinsic function toUTCString() : string {
            /* "Sat, 16 Dec 2006 08:06:21 GMT" */
            return (dayNames[UTCDay] + ", " +
                    twoDigit(UTCDate) + " " +
                    monthNames[UTCMonth] + " " +
                    UTCFullYear + " " +
                    twoDigit(UTCHours) + ":" +
                    twoDigit(UTCMinutes) + ":" +
                    twoDigit(UTCSeconds) + " GMT");
        }

        /* E262-3 15.9.5.3: Date.prototype.toDateString */
        prototype function toDateString(this:Date)
            this.toDateString();

        /* INFORMATIVE */
        intrinsic function toDateString() : string {
            /* "Sat, 16 Dec 2006" */
            return (dayNames[day] + ", " +
                    twoDigit(date) + " " +
                    monthNames[month] + " " +
                    fullYear);
        }

        /* E262-3 15.9.5.4: Date.prototype.toTimeString */
        prototype function toTimeString(this:Date)
            this.toTimeString();

        /* INFORMATIVE */
        intrinsic function toTimeString():string
            /* "00:13:29 GMT-0800" */
            let (tz:double = timezoneOffset)
                let (atz:double = Math.abs(tz))
                    twoDigit(hours) + ":" + twoDigit(minutes) + ":" + twoDigit(seconds) +
                    " GMT" + signString(tz) + twoDigit(Math.floor(atz / 60)) + twoDigit(atz % 60);

        /* E262-3 15.9.5.5: Date.prototype.toLocaleString */
        prototype function toLocaleString(this:Date)
            this.toLocaleString();

        /* INFORMATIVE */
        override intrinsic function toLocaleString() : string
            toString();

        /* E262-3 15.9.5.6: Date.prototype.toLocaleDateString */
        prototype function toLocaleDateString(this:Date)
            this.toLocaleDateString();

        /* INFORMATIVE */
        intrinsic function toLocaleDateString() : string
            toDateString();

        /* E262-3 15.9.5.7: Date.prototype.toLocaleTimeString */
        prototype function toLocaleTimeString(this:Date)
            this.toLocaleTimeString();

        /* INFORMATIVE */
        intrinsic function toLocaleTimeString() : string
            toTimeString();

        /* E262-3 15.9.5.8: Date.prototype.valueOf */
        prototype function valueOf(this:Date)
            this.valueOf();

        override intrinsic function valueOf() : Object
            getTime();

        /* E262-4 proposals:date_and_time: the logical components of
           date values are gettable and settable through properties
           naming those components, and everything else is implemented
           in terms of those components.

           Brendan and Lars agreed (2006-12-15) that these getters and
           setters are not overridable by assignment, ie, they're on
           the Date instances, not properties on the prototype.

           As the Date class is final the getters and setters can be
           inlined. 
        */

        function get time(this:Date) : double 
            getTime();
        function set time(this:Date, t : double) : double 
            setTime(t);

        function get year(this:Date) : double 
            getYear();
        function set year(this:Date, t: double) : double
            setYear(t);

        function get fullYear(this:Date) : double 
            getFullYear();
        function set fullYear(this:Date, t : double) : double 
            setFullYear(t);

        function get UTCFullYear(this:Date) : double 
            getUTCFullYear();
        function set UTCFullYear(this:Date, t : double) : double 
            setUTCFullYear(t);

        function get month(this:Date) : double 
            getMonth();
        function set month(this:Date, t : double) : double 
            setMonth(t);

        function get UTCMonth(this:Date) : double 
            getUTCMonth();
        function set UTCMonth(this:Date, t : double) : double 
            setUTCMonth(t);

        function get date(this:Date) : double 
            getDate();
        function set date(this:Date, t : double) : double 
            setDate(t);

        function get UTCDate(this:Date) : double 
            getUTCDate();
        function set UTCDate(this:Date, t : double) : double 
            setUTCDate(t);

        function get day(this:Date) : double 
            getDay();

        function get UTCDay(this:Date) : double 
            getUTCDay();

        function get hours(this:Date) : double 
            getHours();
        function set hours(this:Date, t : double) : double 
            setHours(t);

        function get UTCHours(this:Date) : double 
            getUTCHours();
        function set UTCHours(this:Date, t : double) : double 
            setUTCHours(t);

        function get minutes(this:Date) : double 
            getMinutes();
        function set minutes(this:Date, t : double) : double 
            setMinutes(t);

        function get UTCMinutes(this:Date) : double 
            getUTCMinutes();
        function set UTCMinutes(this:Date, t : double) : double 
            setUTCMinutes(t);

        function get seconds(this:Date) : double 
            getSeconds();
        function set seconds(this:Date, t : double) : double 
            setSeconds(t);

        function get UTCSeconds(this:Date) : double 
            getUTCSeconds();
        function set UTCSeconds(this:Date, t : double) : double 
            setUTCSeconds(t);

        function get milliseconds(this:Date) : double 
            getMilliseconds();
        function set milliseconds(this:Date, t : double) : double 
            setMilliseconds(t);

        function get UTCMilliseconds(this:Date) : double 
            getUTCMilliseconds();
        function set UTCMilliseconds(this:Date, t : double) : double 
            setUTCMilliseconds(t);

        function get timezoneOffset(this:Date) : double            
            getTimezoneOffset();


        /* Mandated aliases for the canonical getters and setters */

        /* E262-3 15.9.5.9: Date.prototype.getTime */
        prototype function getTime(this:Date)
            this.intrinsic::getTime();

        intrinsic function getTime() : double
            timeval;

        /* E262-3 B.2.4: Date.prototype.getYear */
        prototype function getYear(this:Date)
            this.intrinsic::getYear();

        intrinsic function getYear() : double {
            let t = timeval;
            return isNaN(t) ? t : YearFromTime(LocalTime(t)) - 1900;
        }

        /* E262-3 15.9.5.10: Date.prototype.getFullYear */
        prototype function getFullYear(this:Date)
            this.intrinsic::getFullYear();

        intrinsic function getFullYear() : double
            let (t = timeval)
                isNaN(t) ? t : YearFromTime(LocalTime(t));

        /* E262-3 15.9.5.11: Date.prototype.getUTCFullYear */
        prototype function getUTCFullYear(this:Date)
            this.intrinsic::getUTCFullYear();

        intrinsic function getUTCFullYear() : double
            let (t = timeval)
                isNaN(t) ? t : YearFromTime(t);

        /* E262-3 15.9.5.12: Date.prototype.getMonth */
        prototype function getMonth(this:Date)
            this.intrinsic::getMonth();

        intrinsic function getMonth() : double
            let (t = timeval)
                isNaN(t) ? t : MonthFromTime(LocalTime(t));

        /* E262-3 15.9.5.13: Date.prototype.getUTCMonth */
        prototype function getUTCMonth(this:Date)
            this.intrinsic::getUTCMonth();

        intrinsic function getUTCMonth() : double
            let (t = timeval)
                isNaN(t) ? t : MonthFromTime(t);

        /* E262-3 15.9.5.14: Date.prototype.getDate */
        prototype function getDate(this:Date)
            this.intrinsic::getDate();

        intrinsic function getDate() : double
            let (t = timeval)
                isNaN(t) ? t : DateFromTime(LocalTime(t));

        /* E262-3 15.9.5.15: Date.prototype.getUTCDate */
        prototype function getUTCDate(this:Date)
            this.intrinsic::getUTCDate();

        intrinsic function getUTCDate() : double
            let (t = timeval)
                isNaN(t) ? t : DateFromTime(t);

        /* E262-3 15.9.5.16: Date.prototype.getDay */
        prototype function getDay(this:Date)
            this.intrinsic::getDay();

        intrinsic function getDay() : double
            let (t = timeval)
                isNaN(t) ? t : WeekDay(LocalTime(t));

        /* E262-3 15.9.5.17: Date.prototype.getUTCDay */
        prototype function getUTCDay(this:Date)
            this.intrinsic::getUTCDay();

        intrinsic function getUTCDay() : double
            let (t = timeval)
                isNaN(t) ? t : WeekDay(t);

        /* E262-3 15.9.5.18: Date.prototype.getHours */
        prototype function getHours(this:Date)
            this.intrinsic::getHours();

        intrinsic function getHours() : double
            let (t = timeval)
                isNaN(t) ? t : HourFromTime(LocalTime(t));

        /* E262-3 15.9.5.19: Date.prototype.getUTCHours */
        prototype function getUTCHours(this:Date)
            this.intrinsic::getUTCHours();

        intrinsic function getUTCHours() : double
            let (t = timeval)
                isNaN(t) ? t : HourFromTime(t);

        /* E262-3 15.9.5.20: Date.prototype.getMinutes */
        prototype function getMinutes(this:Date)
            this.intrinsic::getMinutes();

        intrinsic function getMinutes() : double
            let (t = timeval)
                isNaN(t) ? t : MinFromTime(LocalTime(t));

        /* E262-3 15.9.5.21: Date.prototype.getUTCMinutes */
        prototype function getUTCMinutes(this:Date)
            this.intrinsic::getUTCMinutes();

        intrinsic function getUTCMinutes() : double
            let (t = timeval)
                isNaN(t) ? t : MinFromTime(t);

        /* E262-3 15.9.5.22: Date.prototype.getSeconds */
        prototype function getSeconds(this:Date)
            this.intrinsic::getSeconds();

        intrinsic function getSeconds() : double
            let (t = timeval)
                isNaN(t) ? t : SecFromTime(LocalTime(t));

        /* E262-3 15.9.5.23: Date.prototype.getUTCSeconds */
        prototype function getUTCSeconds(this:Date)
            this.intrinsic::getUTCSeconds();

        intrinsic function getUTCSeconds() : double
            let (t = timeval)
                isNaN(t) ? t : SecFromTime(t);

        /* E262-3 15.9.5.24: Date.prototype.getMilliseconds */
        prototype function getMilliseconds(this:Date)
            this.intrinsic::getMilliseconds();

        intrinsic function getMilliseconds() : double
            let (t = timeval)
                isNaN(t) ? t : msFromTime(LocalTime(t));

        /* E262-3 15.9.5.25: Date.prototype.getUTCMilliseconds */
        prototype function getUTCMilliseconds(this:Date)
            this.intrinsic::getUTCMilliseconds();

        intrinsic function getUTCMilliseconds() : double
            let (t = timeval)
                isNaN(t) ? t : msFromTime(t);

        /* E262-3 15.9.5.26: Date.prototype.getTimezoneOffset */
        prototype function getTimezoneOffset(this:Date)
            this.intrinsic::getTimezoneOffset();

        intrinsic function getTimezoneOffset() : double
            let (t = timeval)
                isNaN(t) ? t : (t - LocalTime(t)) / msPerMinute;

        /* E262-3 15.9.5.27: Date.prototype.setTime */
        prototype function setTime(this:Date, t)
            this.intrinsic::setTime(double(t));

        intrinsic function setTime(t:double) : double
            timeval = TimeClip(t);

        /* E262-3 15.9.5.28:  Date.prototype.setMilliseconds (ms) */
        prototype function setMilliseconds(this:Date, ms)
            this.intrinsic::setMilliseconds(double(ms))

        intrinsic function setMilliseconds(ms:double) : double
            timeval = let (t = LocalTime(timeval))
                          UTCTime(MakeDate(Day(t), MakeTime(HourFromTime(t),
                                                            MinFromTime(t),
                                                            SecFromTime(t),
                                                            ms)));

        // 15.9.5.29 Date.prototype.setUTCMilliseconds (ms)
        prototype function setUTCMilliseconds(this:Date, ms)
            this.intrinsic::setUTCMilliseconds(double(ms));

        intrinsic function setUTCMilliseconds(ms:double) : double
            timeval = let (t = timeval)
                          MakeDate(Day(t), MakeTime(HourFromTime(t),
                                                    MinFromTime(t),
                                                    SecFromTime(t),
                                                    ms));

        // 15.9.5.30 Date.prototype.setSeconds (sec [, ms ] )
        prototype function setSeconds(this:Date, sec, ms = this.getMilliseconds())
            this.intrinsic::setSeconds(double(sec), double(ms));

        intrinsic function setSeconds(sec:double, ms:double = getMilliseconds()) : double
            timeval = let (t = LocalTime(timeval))
                          UTCTime(MakeDate(Day(t), MakeTime(HourFromTime(t),
                                                            MinFromTime(t),
                                                            sec,
                                                            ms)));

        // 15.9.5.31 Date.prototype.setUTCSeconds (sec [, ms ] )
        prototype function setUTCSeconds(this:Date, sec, ms = this.getUTCMilliseconds())
            this.intrinsic::setUTCSeconds(double(sec), double(ms));

        intrinsic function setUTCSeconds(sec:double, ms:double = getUTCMilliseconds()) : double
            timeval = let (t = timeval)
                          MakeDate(Day(t), MakeTime(HourFromTime(t),
                                                    MinFromTime(t),
                                                    sec,
                                                    ms));

        // 15.9.5.33 Date.prototype.setMinutes (min [, sec [, ms ] ] )
        prototype function setMinutes(this:Date, min, sec = this.getSeconds(), ms = this.getMilliseconds())
            this.intrinsic::setMinutes(double(min), double(sec), double(ms));

        intrinsic function setMinutes(min:double,
                                      sec:double = getSeconds(),
                                      ms:double = getMilliseconds()) : double
            timeval = let (t = LocalTime(timeval))
                          UTCTime(MakeDate(Day(t), MakeTime(HourFromTime(t),
                                                            min,
                                                            sec,
                                                            ms)));

        // 15.9.5.34 Date.prototype.setUTCMinutes (min [, sec [, ms ] ] )
        prototype function setUTCMinutes(this:Date, 
                                         min, 
                                         sec = this.getUTCSeconds(), 
                                         ms = this.getUTCMilliseconds())
            this.intrinsic::setUTCMinutes(double(min), double(sec), double(ms));

        intrinsic function setUTCMinutes(min:double,
                                         sec:double = getUTCSeconds(),
                                         ms:double = getUTCMilliseconds()) : double
            timeval = let (t = timeval)
                          MakeDate(Day(t), MakeTime(HourFromTime(t),
                                                    min,
                                                    sec,
                                                    ms));

        // 15.9.5.35 Date.prototype.setHours (hour [, min [, sec [, ms ] ] ] )
        prototype function setHours(this:Date, 
                                    hour, 
                                    min=this.getMinutes(), 
                                    sec=this.getSeconds(), 
                                    ms=this.getMilliseconds())
            this.intrinsic::setHours(double(hour), double(min), double(sec), double(ms));

        intrinsic function setHours(hour: double,
                                    min: double = getMinutes(),
                                    sec: double = getSeconds(),
                                    ms: double = getMilliseconds()) : double
            timeval = let (t = LocalTime(timeval))
                          UTCTime(MakeDate(Day(t), MakeTime(hour,
                                                            min,
                                                            sec,
                                                            ms)));

        // 15.9.5.36 Date.prototype.setUTCHours (hour [, min [, sec [, ms ] ] ] )
        prototype function setUTCHours(this:Date, 
                                       hour, 
                                       min=this.getUTCMinutes(), 
                                       sec=this.getUTCSeconds(), 
                                       ms=this.getUTCMilliseconds())
            this.intrinsic::setUTCHours(double(hour), double(min), double(sec), double(ms));

        intrinsic function setUTCHours(hour: double,
                                       min: double = getUTCMinutes(),
                                       sec: double = getUTCSeconds(),
                                       ms: double = getUTCMilliseconds()) : double
            timeval = let (t = timeval)
                          MakeDate(Day(t), MakeTime(hour,
                                                    min,
                                                    sec,
                                                    ms));

        // 15.9.5.36 Date.prototype.setDate (date)
        prototype function setDate(this:Date, date)
            this.intrinsic::setDate(double(date));

        intrinsic function setDate(date: double): double
            timeval = let (t = LocalTime(timeval))
                          UTCTime(MakeDate(MakeDay(YearFromTime(t), MonthFromTime(t), date),
                                           TimeWithinDay(t)));


        // 15.9.5.37 Date.prototype.setUTCDate (date)
        prototype function setUTCDate(this:Date, date)
            this.intrinsic::setUTCDate(double(date));

        intrinsic function setUTCDate(date: double): double
            timeval = let (t = timeval)
                          MakeDate(MakeDay(YearFromTime(t), MonthFromTime(t), date),
                                   TimeWithinDay(t));


        // 15.9.5.38 Date.prototype.setMonth (month [, date ] )
        prototype function setMonth(this:Date, month, date=this.getDate())
            this.intrinsic::setMonth(double(month), double(date));

        intrinsic function setMonth(month:double, date:double = getDate()):double
            timeval = let (t = LocalTime(timeval))
                          UTCTime(MakeDate(MakeDay(YearFromTime(t), month, date),
                                           TimeWithinDay(t)));

        /* E262-3 15.9.5.39: Date.prototype.setUTCMonth */
        prototype function setUTCMonth(this:Date, month, date=this.getUTCDate())
            this.intrinsic::setUTCMonth(double(month), double(date));

        intrinsic function setUTCMonth(month:double, date:double = getUTCDate()):double
            timeval = let (t = timeval)
                          MakeDate(MakeDay(YearFromTime(t), month, date),
                                   TimeWithinDay(t));

        /* E262-3 15.9.5.40: Date.prototype.setFullYear */
        prototype function setFullYear(this:Date, year, month=this.getMonth(), date=this.getDate())
            this.intrinsic::setFullYear(double(year), double(month), double(date));

        intrinsic function setFullYear(year:double,
                                       month:double = getMonth(),
                                       date:double = getDate()) : double
            timeval = let (t = LocalTime(timeval))
                          UTCTime(MakeDate(MakeDay(year, month, date),
                                           TimeWithinDay(t)));

        /* E262-3 15.9.5.41: Date.prototype.setUTCFullYear */
        prototype function setUTCFullYear(this:Date, year, month=this.getUTCMonth(), date=this.getUTCDate())
            this.intrinsic::setUTCFullYear(double(year), double(month), double(date));

        intrinsic function setUTCFullYear(year:double,
                                          month:double = getUTCMonth(),
                                          date:double = getUTCDate()) : double
            timeval = let (t = timeval)
                          MakeDate(MakeDay(year, month, date),
                                   TimeWithinDay(t));


        /* E262-3 B.2.5: Date.prototype.setYear */
        prototype function setYear(this:Date, year)
            this.intrinsic::setYear(double(year));

        intrinsic function setYear(this:Date, year:double) {
            let t = isNaN(timeval) ? 0.0 : LocalTime(timeval);
            if (isNaN(year)) {
                timeval = NaN;
                return NaN;
            }
            let y = helper::toInteger(year);
            if (y < 0 || y > 99)
                y += 1900;
            timeval = TimeClip(UTC(MakeDate(MakeDay(y, MonthFromTime(t), DateFromTime(t)),
                                            TimeWithinDay(t))));
            return timeval;
        }

        /* Primitives from E262-3 */
        
        private var timeval : double;    // This object's time value
        private var birthtime : double;  // INFORMATIVE.  For use by nanoAge
    }


    informative const dayNames = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
    informative const monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

    /* Format of string produced by Date.toString, recognized by Date.parse */
    /* e.g., "Fri, 15 Dec 2006 23:45:09 GMT-0800" */

    informative const adhocTimestamp : RegExp! =
        /(?: Mon|Tue|Wed|Thu|Fri|Sat|Sun ),?\s+
         (?P<day> [0-9]+ )\s+
         (?P<month> Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec )\s+
         (?P<year> -? [0-9]+ )\s+
         (?P<hour> [0-9]{2} ):
         (?P<minute> [0-9]{2} ):
         (?P<second> [0-9]{2} )\s+
         GMT
         (?P<tz> (?: \+ | - ) [0-9]{4} )?/x;

    /* Most practical implementations have many heuristics for
       parsing date formats, which are as numerous as the sands of
       the desert, the snowflakes of the arctic, the stars in the
       sky, etc.  Parsing the output of Date.prototype.toString is
       the minimum required by the Standard and we handle that
       here along with the output of Date.prototype.toUTCString.
    */
    informative function fromDateString(s : string, reference : double) : double {

        function findMonth(name) {
            for ( let i:double=0 ; i < monthNames.length ; i++ )
                if (name === monthNames[i])
                    return i;
            return 0;  // implementation bug if this happens...
        }

        let res = adhocTimestamp.exec(s);
        if (res === null || res === undefined)
            return reference;
        let t = Date.UTC(parseInt(res.year),
                         findMonth(res.month),
                         parseInt(res.day),
                         parseInt(res.hour),
                         parseInt(res.minute),
                         parseInt(res.second));
        if (res.tz !== undefined) {
            let hour = parseInt(res.tz.substring(1,3)); // FIXME #132: should be [1:3]
            let min  = parseInt(res.tz.substring(3,5)); // FIXME #132: should be [3:5]
            if (res.tz[0] == '+')
                t -= (hour*60 + min)*60*1000;
            else
                t += (hour*60 + min)*60*1000;
        }
        return t;
    }

    informative function twoDigit(n : double)
        string(n + 100).substring(1,3);

    informative function signString(n : double)
        n < 0 ? "-" : "+";

    helper const hoursPerDay = 24;
    helper const minutesPerHour = 60;
    helper const secondsPerMinute = 60;
    helper const daysPerYear = 365.2425;

    helper const msPerSecond = 1000;
    helper const msPerMinute = msPerSecond * secondsPerMinute;
    helper const msPerHour = msPerMinute * minutesPerHour;
    helper const msPerDay = msPerHour * hoursPerDay;
    helper const msPerYear = msPerDay * daysPerYear;

    helper const monthOffsets = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];

    /* Format of string produced by Date.toISOString, recognized by Date.parse */
    /* e.g, "2006-12-15T23:45:09.33-08:00" */

    helper const isoTimestamp =
        /^
         # Date, optional
         (?: (?P<year> - [0-9]+ | [0-9]{4} [0-9]* )
          (?: - (?P<month> [0-9]{2} )
           (?: - (?P<day> [0-9]{2} ) )? )? )?
         T
         # Time, optional
         (?: (?P<hour> [0-9]{2} )
          (?: : (?P<minutes> [0-9]{2} )
           (?: : (?P<seconds> [0-9]{2} )
            (?: \. (?P<fraction> [0-9]+ ) )? )? )? )?
         # Timezone, optional
         (?: (?P<zulu> Z )
          | (?P<offs>
             (?P<tzdir> \+ | - )
             (?P<tzhr> [0-9]{2} )
             (?: : (?P<tzmin> [0-9]{2} ) )? ) )?
         $/x;

    helper function MakeTime(hour:double, min:double, sec:double, ms:double ):double {
        if (!isFinite(hour) || !isFinite(min) || !isFinite(sec) || !isFinite(ms))
            return NaN;

        return (helper::toInteger(hour) * msPerHour +
                helper::toInteger(min) * msPerMinute +
                helper::toInteger(sec) * msPerSecond +
                helper::toInteger(ms));
    }

    helper function MakeDay(year : double, month : double, date : double) : double {
        if (!isFinite(year) || !isFinite(month) || !isFinite(date))
            return NaN;

        year = helper::toInteger(year);
        month = helper::toInteger(month);
        date = helper::toInteger(date);

        /* INFORMATIVE, the spec is non-operational. */
        year += Math.floor(month / 12);

        month = month % 12;
        if (month < 0)
            month += 12;

        let leap = (DaysInYear(year) == 366);

        let yearday = Math.floor(TimeFromYear(year) / msPerDay);
        let monthday = DayFromMonth(month, leap);

        return yearday + monthday + date - 1;
    }

    helper function MakeDate(day : double, time : double) : double {
        if (!isFinite(day) || !isFinite(time))
            return NaN;

        return day * msPerDay + time;
    }

    helper function Day(t : double) : double
        Math.floor(t / msPerDay);

    helper function TimeWithinDay(t : double) : double
        t % msPerDay;

    helper function HourFromTime(t : double) : double {
        let v = Math.floor(t / msPerHour) % hoursPerDay;
        if (v < 0)
            return v + hoursPerDay;
        return v;
    }

    helper function MinFromTime(t : double) : double {
        let v = Math.floor(t / msPerMinute) % minutesPerHour;
        if (v < 0)
            return v + minutesPerHour;
        return v;
    }

    helper function SecFromTime(t : double) : double {
        let v = Math.floor(t / msPerSecond) % secondsPerMinute;
        if (v < 0)
            return v + secondsPerMinute;
        return v;
    }

    helper function msFromTime(t : double) : double
        t % msPerSecond;
            
    helper function DaysInYear(y : double) : double {
        if (y % 4 !== 0 || y % 100 === 0 && y % 400 !== 0)
            return 365;
        else
            return 366;
    }

    helper function DayFromYear(y : double) : double
        365 * (y-1970) + Math.floor((y-1969)/4) - Math.floor((y-1901)/100) + Math.floor((y-1601)/400);

    helper function TimeFromYear(y : double) : double
        msPerDay * DayFromYear(y);

    helper function InLeapYear(t : double) : double
        (DaysInYear(YearFromTime(t)) == 365) ? 0 : 1;

    helper function MonthFromTime(t : double) : double {
        let dwy = DayWithinYear(t),
            ily = InLeapYear(t);
        for ( let i=monthOffsets.length-1; i >= 0; i-- ) {
            let firstDayOfMonth = monthOffsets[i];
            if (i >= 2)
                firstDayOfMonth += ily;
            if (dwy >= firstDayOfMonth)
                return i;
        }
    }

    helper function DayWithinYear(t : double) : double
        Day(t) - DayFromYear(YearFromTime(t));

    helper function DayFromMonth(m : double, leap : boolean) : double
        monthOffsets[m] + (leap && m >= 2 ? 1 : 0);

    // Days-within-year are 0-based. Dates-within-months
    // are 1-based. Hence the dwy+1 below: day 0 == Jan 1.
    helper function DateFromTime(t : double) : double {
        let dwy = DayWithinYear(t),
            mft = MonthFromTime(t),
            ily = InLeapYear(t);
        return (dwy+1) - (monthOffsets[mft]) - (mft >= 2 ? ily : 0);
    }

    helper function WeekDay(t : double) : double {
        let v = (Day(t) + 4) % 7;
        if (v < 0)
            return v + 7;
        return v;
    }

    helper function LocalTime(t : double) : double
        t + LocalTZA() + DaylightSavingsTA(t);

    helper function UTCTime(t : double) : double
        t - LocalTZA() - DaylightSavingsTA(t - LocalTZA());

    helper function TimeClip(t : double) : double
        (!isFinite(t) || Math.abs(t) > 8.64e15) ? NaN : adjustZero(helper::toInteger(t));

    // This can return t or t + (+0.0), and it is a specification hack to allow
    // time values to actually be represented by 64-bit ints (which can't represent
    // -0.0).  Adding +0.0 turns -0.0 into +0.0.
    informative function adjustZero(t: double): double
        t;

    informative function YearFromTime(t : double) : double {
        let y  = Math.floor(t / msPerYear) + 1970;
        let t2 = TimeFromYear(y);
        if (t2 > t)
            y--;
        else if (t2 + msPerDay * DaysInYear(y) <= t)
            y++;
        return y;
    }

    intrinsic native function LocalTZA() : double;
    intrinsic native function DaylightSavingsTA(t : double) : double;

    helper function formatYears(n: double): string {
        if (n >= 0 && n <= 9999)
            return zeroFill(int(n), 4);
        else
            return string(n);
    }

    helper function removeTrailingZeroes(n: double): string {
        while (n > 0 && n % 10 === 0)
            n /= 10;
        return string(n);
    }

    helper function zeroFill(n: double, k): string {
        let s = string(n);
        while (s.length < k)
            s = "0" + s;
        return s;
    }
