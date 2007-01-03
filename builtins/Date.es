/* -*- mode: java; mode: font-lock; tab-width: 4; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "date" object
 *
 * E262-3 15.9
 * E262-4 proposals:date_and_time
 *
 * Status: incomplete
 */

package
{
    /* The Date class is "final dynamic" in ActionScript 3.0, though
       the motivation for that is unclear.  The consequence is anyway
       that the getters for the components of a Date can be inlined.
    */
    final dynamic class Date extends Object
    {       
        // 15.9.2 The Date Constructor Called as a Function
        static intrinsic function call(...args) {
            // args are ignored.
            return (new Date()).toString();
        }

        /* E262-3 15.9.3: The Date Constructor */
        function Date(year, month, date, hours, minutes, seconds, ms) {
            use namespace intrinsic;

            let argc : int = arguments.length;

            year = ToNumber(year);
            month = ToNumber(month);
            date = argc >= 3 ? ToNumber(date) : 1;
            hours = argc >= 4 ? ToNumber(hours) : 0;
            minutes = argc >= 5 ? ToNumber(minutes) : 0;
            seconds = argc >= 6 ? ToNumber(seconds) : 0;
            ms = argc >= 7 ? ToNumber(ms) : 0;

            var intYear = ToInteger(year);
            if (!isNaN(year) && 0 <= intYear && intYear <= 99)
                intYear += 1900;

            timeval = TimeClip(UTCTime(MakeDate(MakeDay(intYear, month, date), 
                                                MakeTime(hours, minutes, seconds, ms))));
        }

        /* E262-3 15.9.4.2: Date.parse */
        var parse = function parse(string, reference : double=0.0) {
            return Date.intrinsic::parse(String(string), reference);
        }

        // FIXME: INTERPRETATION: Is reference localtime or UTC?
        static intrinsic function parse(s:String!, reference:double=0.0) : Date! {
            use namespace intrinsic;

            function fractionToMilliseconds(frac : String!) : double
                Math.floor(1000 * (parseInt(frac) / Math.pow(10,frac.length)));

            let isoRes : Object = isoTimestamp.intrinsic::exec(s);
            let defaults : Date! = new Date(reference);
            if (isoRes !== null) {
                let year = isoRes.year !== null ? parseInt(isoRes.year) : defaults.UTCYear;
                let month = isoRes.month !== null ? parseInt(isoRes.month) : defaults.UTCMonth;
                let day = isoRes.day !== null ? parseInt(isoRes.day) : defaults.UTCDay;
                let hour = isoRes.hour !== null ? parseInt(isoRes.hour) : defaults.UTCHour;
                let mins = isoRes.mins !== null ? parseInt(isoRes.mins) : defaults.UTCMinutes;
                let secs = isoRes.secs !== null ? parseInt(isoRes.secs) : defaults.UTCSeconds;
                let millisecs = isoRes.fraction !== null ? 
                    fractionToMillisecons(isoRes.fraction) : 
                    defaults.UTCMilliseconds;
                let tzo = defaults.timezoneOffset;
                if (isoRes.zulu !== null)
                    tzo = 0;
                else if (isoRes.offs !== null) {
                    tzo = parseInt(isoRes.tzhr) * 60;
                    if (isoRes.tzmin !== null)
                        tzo += parseInt(isoRes.tzmin);
                    if (isoRes.tzdir === "-")
                        tzo = -tzo;
                }
                return new Date(Date.UTC(year, month, day, hour, mins, secs, millisecs).time - tzo);
            }
            else
                return fromDateString(s, reference);
        }
        
        /* INFORMATIVE.  

           Most practical implementations have many heuristics for
           parsing date formats, which are as numerous as the sands of
           the desert, the snowflakes of the arctic, the stars in the
           sky, etc.  Parsing the output of Date.prototype.toString is
           the minimum required by the Standard and we handle that
           here along with the output of Date.prototype.toUTCString.
        */
        private static function fromDateString(s : String!, reference : double) : Date {
            use namespace intrinsic;

            function findMonth(name) {
                for ( var i=0 ; i < monthNames.length ; i++ )
                    if (name === monthNames[i])
                        return i;
                return 0;  // implementation bug if this happens...
            }

            var res = adhocTimestamp.exec(s);
            if (res === null)
                return new Date(reference);
            var t = Date.UTC(parseInt(res.year),
                             findMonth(res.month),
                             parseInt(res.day),
                             parseInt(res.hour),
                             parseInt(res.minute),
                             parseInt(res.second));
            if (res.tz !== null) {
                // FIXME
            }
            return new Date(t);
        }

        /* E262-4 proposals:date_and_time */
        static intrinsic function now() : double
            Date.now()

        static native function now() : double;

        /* E262-4 proposals:date_and_time */
        static intrinsic function nanoAge() : double
            Date.nanoAge();

        static native function nanoAge() : double;

        /* E262-3 15.9.4.3: Date.UTC */
        var UTC = function UTC(year, month, date, hours, minutes, seconds, ms) {
            use namespace intrinsic;

            var argc = arguments.length;
            return Date.UTC(ToNumber(year), 
                            ToNumber(month), 
                            argc >= 3 ? ToNumber(date) : 1,
                            argc >= 4 ? ToNumber(hours) : 0,
                            argc >= 5 ? ToNumber(minutes) : 0,
                            argc >= 6 ? ToNumber(seconds) : 0,
                            argc >= 7 ? ToNumber(ms) : 0);
        }

        static instrinsic function UTC(year : double, month : double, 
                                       date : double=1, hours : double?=0, minutes : double?=0,
                                       seconds : double=0, ms : double?=0) : double {
            use namespace intrinsic;

            var intYear = ToInteger(year);
            if (!isNaN(year) && 0 <= intYear && intYear <= 99)
                intYear += 1900;
            return TimeClip(MakeDate(MakeDay(intYear, month, date), 
                                     MakeTime(hours, minutes, seconds, ms)));
        }

        private const dayNames = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];
        private const monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

        /* Format of string produced by Date.toString, recognized by Date.parse */
        /* e.g., "Fri, 15 Dec 2006 23:45:09 GMT-0800" */
        private const adhocTimestamp : RegExp! = 
            /(?: Mon|Tue|Wed|Thu|Fri|Sat|Sun )\s+
             (?P<day> [0-9]+ )\s+
             (?P<month> Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec )\s+
             (?P<year> -? [0-9]+ )\s+
             (?P<hour> [0-9]{2} ):
             (?P<minute> [0-9]{2} ):
             (?P<second> [0-9]{2} )\s+
             GMT
             (?P<tz> (?: \\+ | - ) [0-9]{4} )?/x;

        /* Format of string produced by Date.toISO, recognized by Date.parse */
        /* e.g, "2006-12-15T23:45:09.33-08:00" */
        private const isoTimestamp : RegExp! =
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
                (?P<tzdir> \\+ | - )
                (?P<tzhr> [0-9]{2} )
                (?: : (?P<tzmin> [0-9]{2} ) )? ) )?
            $/x;

        /* E262-4 proposals:date_and_time */
        prototype function toISO(this:Date) {
            return this.intrinsic::toISO();
        }

        intrinsic function toISO() : String {
            // FIXME: milliseconds should be minimal string (suppress trailing zeroes)
            // FIXME: the years should be formatted much more elaborately
            let tz = timezoneOffset;
            let atz = Math.abs(tz);
            return "" + fullYear + "-" + twoDigit(month) + "-" + twoDigit(day) +
                "T" + twoDigit(hours) + ":" + twoDigit(minutes) + ":" + twoDigit(seconds) +
                "." + threeDigit(milliseconds) + sign(tz) +
                twoDigit(Math.floor(atz / 60)) + ":" + twoDigit(atz % 60);
        }

        /* E262-3 15.9.5.2: Date.prototype.toString */
        prototype function toString(this:Date) { 
            return this.intrinsic::toString(); 
        }

        /* INFORMATIVE */
        intrinsic function toString() : String {
            /* "Fri, 15 Dec 2006 23:45:09 GMT-0800" */
            let tz = timezoneOffset;
            let atz = Math.abs(tz);
            return dayNames[date] + ", " + twoDigit(day) + " " + monthNames[month] + " " fullYear +
                " " + twoDigit(hours) + ":" + twoDigit(minutes) + ":" + twoDigit(seconds) +
                " GMT" + sign(tz) + twoDigit(Math.floor(atz / 60)) + twoDigit(atz % 60);
        }

        /* E262-3 15.9.5.42: Date.prototype.toUTCString */
        prototype function toUTCString(this:Date) {
            return this.intrinsic::toUTCString(); 
        }

        /* INFORMATIVE */
        intrinsic function toUTCString() : String {
            /* "Sat, 16 Dec 2006 08:06:21 GMT" */
            return dayNames[UTCdate] + ", " + 
                twoDigit(UTCday) + " " + 
                monthNames[UTCmonth] + " " +
                UTCFullYear + " " + 
                twoDigit(UTCHours) + ":" + 
                twoDigit(UTCMinutes) + ":" + 
                twoDigit(UTCSeconds) + " GMT";
        }

        /* E262-3 15.9.5.3: Date.prototype.toDateString */
        prototype function toDateString(this:Date) { 
            return this.intrinsic::toDateString(); 
        }

        /* INFORMATIVE */
        intrinsic function toDateString() : String {
            /* "Sat, 16 Dec 2006" */
            return dayNames[date] + ", " + twoDigit(day) + " " + monthNames[month] + " " fullYear;
        }

        /* E262-3 15.9.5.4: Date.prototype.toTimeString */
        prototype function toTimeString(this:Date) { 
            return this.intrinsic::toTimeString(); 
        }

        /* INFORMATIVE */
        intrinsic function toTimeString():String {
            /* "00:13:29 GMT-0800" */
            let tz = timezoneOffset;
            let atz = Math.abs(tz);
            return twoDigit(hours) + ":" + twoDigit(minutes) + ":" + twoDigit(seconds) +
                " GMT" + sign(tz) + twoDigit(Math.floor(atz / 60)) + twoDigit(atz % 60);
        }

        /* E262-3 15.9.5.5: Date.prototype.toLocaleString */
        prototype function toLocaleString(this:Date) {
            return this.intrinsic::toLocaleString(); 
        }

        /* INFORMATIVE */
        intrinsic function toLocaleString():String {
            return this.intrinsic::toString();
        }

        /* E262-3 15.9.5.6: Date.prototype.toLocaleDateString */
        prototype function toLocaleDateString(this:Date) {
            return this.intrinsic::toLocaleDateString(); 
        }

        /* INFORMATIVE */
        intrinsic function toLocaleDateString():String {
            return this.intrinsic::toDateString();
        }

        /* E262-3 15.9.5.7: Date.prototype.toLocaleTimeString */
        prototype function toLocaleTimeString(this:Date) {
            return this.intrinsic::toLocaleTimeString(); 
        }

        /* INFORMATIVE */
        intrinsic function toLocaleTimeString():String {
            return this.intrinsic::toTimeString();
        }

        /* E262-3 15.9.5.8: Date.prototype.valueOf */
        prototype function valueOf(this:Date) {
            return this.intrinsic::valueOf(); 
        }

        intrinsic function valueOf() : Object
            time;

        /* E262-4 proposals:date_and_time: the logical components of
           date values are gettable and settable through properties
           naming those components, and everything else is implemented
           in terms of those components.

           Brendan and Lars agreed (2006-12-15) that these getters and
           setters are not overridable by assignment, ie, they're on
           the Date instances, not properties on the prototype.

           As the Date class is final the getters and setters can be
           inlined. */

        /* E262-3 15.9.5.9: Date.prototype.getTime */
        function get time() : double
            timeval;

        /* E262-3 15.9.5.27: Date.prototype.setTime */
        function set time(t : double) : double
            timeval = TimeClip(t)

        /* E626-3 15.9.5.10: Date.prototype.getFullYear */
        function get fullYear() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return YearFromTime(LocalTime(t));
        }

        /* E262-3 15.9.5.11: Date.prototype.getUTCFullYear */
        function get UTCFullYear() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return YearFromTime(t);
        }

        /* E262-3 15.9.5.12: Date.prototype.getMonth */
        function get month() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return MonthFromTime(LocalTime(t));
        }

        /* E262-3 15.9.5.13: Date.prototype.getUTCMonth */
        function get UTCMonth() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return MonthFromTime(t);
        }

        /* E262-3 15.9.5.14: Date.prototype.getDate */
        function get date() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return DateFromTime(LocalTime(t));
        }

        /* E262-3 15.9.5.15: Date.prototype.getUTCDate */
        function get UTCDate() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return DateFromTime(t);
        }

        /* E262-3 15.9.5.16: Date.prototype.getDay */
        function get day() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return WeekDay(LocalTime(t));
        }

        /* E262-3 15.9.5.17: Date.prototype.getUTCDay */
        function get UTCDay() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return WeekDay(t);
        }

        /* E262-3 15.9.5.18: Date.prototype.getHours */
        function get hours() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return HourFromTime(LocalTime(t));
        }

        /* E262-3 15.9.5.19: Date.prototype.getUTCHours */
        function get UTCHours() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return HourFromTime(t);
        }

        /* E262-3 15.9.5.20: Date.prototype.getMinutes */
        function get minutes() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return MinFromTime(LocalTime(t));
        }

        /* E262-3 15.9.5.21: Date.prototype.getUTCMinutes */
        function get UTCMinutes() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return MinFromTime(t);
        }

        /* E262-3 15.9.5.22: Date.prototype.getSeconds */
        function get seconds() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return SecFromTime(LocalTime(t));
        }

        /* E262-3 15.9.5.23: Date.prototype.getUTCSeconds */
        function get seconds() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return SecFromTime(t);
        }

        /* E262-3 15.9.5.24: Date.prototype.getMilliseconds */
        function get milliseconds() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return msFromTime(LocalTime(t));
        }

        /* E262-3 15.9.5.25: Date.prototype.getUTCMilliseconds */
        function get UTCMilliseconds() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return msFromTime(t);
        }

        /* E262-3 15.9.5.26: Date.prototype.getTimezoneOffset */
        function get timezoneOffset() : double {
            var t : double = timeval;
            if (intrinsic::isNaN(t))
                return t;
            return (t - LocalTime(t)) / msPerMinute;
        }


        /*** Mandated aliases for the canonical getters and setters ***/

        prototype function getTime(this:Date)   time;
        intrinsic function getTime() : double   time;

        prototype function getFullYear(this:Date)   fullYear;
        intrinsic function getFullYear() : double   fullYear;

        prototype function getUTCFullYear(this:Date)   UTCFullYear;
        intrinsic function getUTCFullYear() : double   UTCFullYear;

        prototype function getMonth(this:Date)   month;
        intrinsic function getMonth() : double   month;
        
        prototype function getUTCMonth(this:Date)   UTCMonth;
        intrinsic function getUTCMonth() : double   UTCMonth;

        prototype function getDate(this:Date)   date;
        intrinsic function getDate() : double   date;

        prototype function getUTCDate(this:Date)   UTCDate;
        intrinsic function getUTCDate() : double   UTCDate;

        prototype function getDay(this:Date)   day;
        intrinsic function getDay() : double   day;

        prototype function getUTCDay(this:Date)   UTCDay;
        intrinsic function getUTCDay() : double   UTCDay;

        prototype function getHours(this:Date)   hours;
        intrinsic function getHours() : double   hours;

        prototype function getUTCHours(this:Date)   UTCHours;
        intrinsic function getUTCHours() : double   UTCHours;

        prototype function getMinutes(this:Date)   minutes;
        intrinsic function getMinutes() : double   minutes;
        
        prototype function getUTCMinutes(this:Date)   UTCMinutes;
        intrinsic function getUTCMinutes() : double   UTCMinutes;

        prototype function getSeconds(this:Date)   seconds;
        intrinsic function getSeconds() : double   seconds;

        prototype function getUTCSeconds(this:Date)   UTCSeconds;
        intrinsic function getUTCSeconds() : double   UTCSeconds;

        prototype function getMilliseconds(this:Date)   milliseconds;
        intrinsic function getMilliseconds() : double   milliseconds;
        
        prototype function getUTCMilliseconds(this:Date)   UTCMilliseconds;
        intrinsic function getUTCMilliseconds() : double   UTCMilliseconds;
        
        prototype function getTimezoneOffset(this:Date)   timezoneOffset;
        intrinsic function getTimezoneOffset() : double   timezoneOffset;

        prototype function setTime(this:Date, t)        time = double(t);
        intrinsic function setTime(t:double) : double   time = t;

        /* E262-3 15.9.5.28:  Date.prototype.setMilliseconds (ms) */
        prototype function setMilliseconds(this:Date, ms)
            this.intrinsic::setMilliseconds(double(ms))

        intrinsic function setMilliseconds(ms:double) : double
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(Day(t), MakeTime(HourFromTime(t), 
                                                            MinFromTime(t), 
                                                            SecFromTime(t), 
                                                            ms)));

        // 15.9.5.29 Date.prototype.setUTCMilliseconds (ms)
        prototype function setUTCMilliseconds(this:Date, ms)
            this.intrinsic::setUTCMilliseconds(double(ms));

        intrinsic function setUTCMilliseconds(ms:double) : double
            timeval = let (t : double = timeval)
                          MakeDate(Day(t), MakeTime(HourFromTime(t), 
                                                    MinFromTime(t), 
                                                    SecFromTime(t), 
                                                    ms));

        // 15.9.5.30 Date.prototype.setSeconds (sec [, ms ] )
        prototype function setSeconds(this:Date, sec, ms)
            this.intrinsic::setSeconds(double(sec), double(ms)); 

        intrinsic function setSeconds(sec:double, ms:double = milliseconds) : double
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(Day(t), MakeTime(HourFromTime(t), 
                                                            MinFromTime(t), 
                                                            sec, 
                                                            ms)));

        // 15.9.5.31 Date.prototype.setUTCSeconds (sec [, ms ] )
        prototype function setUTCSeconds(this:Date, sec, ms) 
            this.intrinsic::setUTCSeconds(double(sec), double(ms)); 

        intrinsic function setUTCSeconds(sec:double, ms:double = milliseconds) : double
            timeval = let (t : double = timeval)
                          MakeDate(Day(t), MakeTime(HourFromTime(t), 
                                                    MinFromTime(t), 
                                                    sec, 
                                                    ms));

        // 15.9.5.33 Date.prototype.setMinutes (min [, sec [, ms ] ] )
        prototype function setMinutes(this:Date, min, sec, ms)
            this.intrinsic::setMinutes(double(min), double(sec), double(ms)); 

        intrinsic function setMinutes(min:double, 
                                      sec:double = seconds, 
                                      ms:double = milliseconds) : double 
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(Day(t), MakeTime(HourFromTime(t), 
                                                            min, 
                                                            sec, 
                                                            ms)));

        // 15.9.5.34 Date.prototype.setUTCMinutes (min [, sec [, ms ] ] )
        prototype function setUTCMinutes(this:Date, min, sec, ms)
            this.intrinsic::setUTCMinutes(double(min), double(sec), double(ms)); 

        intrinsic function setUTCMinutes(min:double, 
                                         sec:double = seconds,
                                         ms:double = milliseconds) : double 
            timeval = let (t : double = timeval)
                          MakeDate(Day(t), MakeTime(HourFromTime(t), 
                                                    min, 
                                                    sec, 
                                                    ms));

        // 15.9.5.35 Date.prototype.setHours (hour [, min [, sec [, ms ] ] ] )
        prototype function setHours(this:Date, hour, min, sec, ms)
            this.intrinsic::setHours(double(hour), double(min), double(sec), double(ms));

        intrinsic function setHours(hour:double, 
                                    min:double = minutes,
                                    sec:double = seconds,
                                    ms:double = milliseconds) : double 
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(Day(t), MakeTime(hour, 
                                                            min, 
                                                            sec, 
                                                            ms)));

        // 15.9.5.36 Date.prototype.setUTCHours (hour [, min [, sec [, ms ] ] ] )
        prototype function setUTCHours(this:Date, hour, min, sec, ms)
            this.intrinsic::setUTCHours(double(hour), double(min), double(sec), double(ms));

        intrinsic function setUTCHours(hour:double, 
                                       min:double = minutes,
                                       sec:double = seconds,
                                       ms:double = milliseconds) : double 
            timeval = let (t : double = timeval)
                          MakeDate(Day(t), MakeTime(hour, 
                                                    min, 
                                                    sec, 
                                                    ms));

        // 15.9.5.36 Date.prototype.setDate (date)
        prototype function setDate(this:Date, date)
            this.intrinsic::setDate(double(date)); 

        intrinsic function setDate(date:double):double
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(MakeDay(YearFromTime(t), MonthFromTime(t), date), 
                                           TimeWithinDay(t)));
            

        // 15.9.5.37 Date.prototype.setUTCDate (date)
        prototype function setUTCDate(this:Date, date)
            this.intrinsic::setUTCDate(double(date)); 

        intrinsic function setUTCDate(date:double):double 
            timeval = let (t : double = timeval)
                          MakeDate(MakeDay(YearFromTime(t), MonthFromTime(t), date), 
                                   TimeWithinDay(t));


        // 15.9.5.38 Date.prototype.setMonth (month [, date ] )
        prototype function setMonth(this:Date, month, date)
            this.intrinsic::setMonth(double(month), double(date)); 

        intrinsic function setMonth(month:double, date:double = date):double
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(MakeDay(YearFromTime(t), month, date), 
                                           TimeWithinDay(t)));

        /* E262-3 15.9.5.39: Date.prototype.setUTCMonth */
        prototype function setUTCMonth(this:Date, month, date)
            this.intrinsic::setUTCMonth(double(month), double(date)); 

        intrinsic function setUTCMonth(month:double, date:double = date):double
            timeval = let (t : double = timeval)
                          MakeDate(MakeDay(YearFromTime(t), month, date), 
                                   TimeWithinDay(t));

        /* E262-3 15.9.5.40: Date.prototype.setFullYear */
        prototype function setFullYear(this:Date, year, month, date)
            this.intrinsic::setFullYear(double(year), double(month), double(date)); 

        intrinsic function setFullYear(year:double, 
                                       month:double = month, 
                                       date:double = date) : double 
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(MakeDay(year, month, date), 
                                           TimeWithinDay(t)));

        /* E262-3 15.9.5.41: Date.prototype.setUTCFullYear */
        prototype function setUTCFullYear(this:Date, year, month, date)
            this.intrinsic::setUTCFullYear(double(year), double(month), double(date)); 

        intrinsic function setUTCFullYear(year:double, 
                                          month:double = month, 
                                          date:double = date) : double 
            timeval = let (t : double = timeval)
                          MakeDate(MakeDay(year, month, date), 
                                   TimeWithinDay(t));


        /*** Utilities ***/

        private function twoDigit(n : double)
            (n + 100).toString().substring(1);

        private function threeDigit(n : double)
            (n + 1000).toString().substring(1);

        private function sign(n : double)
            n < 0 ? "-" : "+";

        /*** Primitives from E262-3 ***/

        private var timeval : double = 0;

        private const msPerDay : double = 86400000;
        private const hoursPerDay : double = 24;
        private const minutesPerHour : double = 60;
        private const secondsPerMinute : double = 60;
        private const msPerSecond : double = 1000;
        private const msPerMinute : double = msPerSecond * secondsPerMinute;
        private const msPerHour : double = msPerMinute * minutesPerHour;
        private const monthOffsets : [double] 
            = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334] : [double];

        private function MakeTime(hour:double, min:double, sec:double, ms:double ):double {
            use namespace intrinsic;

            if (!isFinite(hour) || !isFinite(min) || !isFinite(sec) || !isFinite(ms))
                return NaN;

            return ToInteger(hour) * msPerHour +
                ToInteger(min) * msPerMinute +
                ToInteger(sec) * msPerSecond +
                ToInteger(ms);
        }

        private function MakeDay(year : double, month : double, date : double) : double {
            use namespace intrinsic;

            if (!isFinite(year) || !isFinite(month) || !isFinite(date))
                return NaN;

            year = ToInteger(year);
            month = ToInteger(month);
            date = ToInteger(date);
            // FIXME
        }

        private function MakeDate(day : double, time : double) : double {
            use namespace intrinsic;

            if (!isFinite(day) || !isFinite(time))
                return NaN;
            return day * msPerDay + time;
        }

        private function Day(t : double) : double
            Math.floor(t / msPerDay);

        private function TimeWithinDay(t : double) : double
            t % msPerDay;

        private function HoursFromTime(t : double) : double
            Math.floor(t / msPerHour) % hoursPerDay;
        
        private function DaysInYear(y : double) : double {
            if (y % 4 !== 0) return 365;
            if (y % 100 !== 0) return 366;
            if (y % 400 !== 0) return 365;
            return 366;
        }

        private function DayFromYear(y : double) : double {
            use namespace intrinsic;

            return 365*(y-1970) +
                Math.floor((y-1969)/4) -
                Math.floor((y-1901)/100) + 
                Math.floor((y-1601)/400);
        }

        private function TimeFromYear(y : double) : double
            msPerDay * DayFromYear(y);

        private function InLeapYear(t : double) : double
            DaysInYear(YearFromTime(t)) ? 1 : 0;

        private function MonthFromTime(t : double) : double {
            let dwy : double = DayWithinYear(t),
                ily : double = InLeapYear(t);
            for ( var i : int=0 ; i < monthOffsets.length-1 ; i++ )
                if (dwy > monthOffsets[i] + (i >= 2 ? ily : 0) && 
                    dwy < monthOffsets[i+1] + (i+1 >= 2 ? ily : 0))
                    return i;
            /*NOTREACHED*/
        }

        private function DayWithinYear(t : double) : double
            Day(t) - DayFromYear(YearFromTime(t));

        private function DateFromTime(t : double) : double {
            let dwy : double = DayWithinYear(t),
                mft : double = MonthFromTime(t),
                ily : double = InLeapYear(t);
            return dwy - (monthOffsets[mft] - 1) - (mft >= 2 ? ily : 0);
        }

        private function WeekDay(t : double) : double
            (Day(t) + 4) % 7;

        private function LocalTime(t : double) : double
            t + LocalTZA() + DaylightSavingTA(t);

        private function UTCTime(t : double) : double
            t - LocalTZA() - DaylighSavingsTA(t - LocalTZA());

        private function TimeClip(t : double) : double {
            use namespace intrinsic;

            if (!isFinite(t) || Math.abs(t) > 8.64e15)
                return NaN;
            else
                return ToInteger(t);
        }

        /* INFORMATIVE */
        private function YearFromTime(t : double) : double {
            let y : double = t / (msPerDay * 365);
            while (TimeFromYear(y) < t)
                y += 1;
            while (TimeFromYear(y) > t)
                y -= 1;
            return y;
        }

        private native function LocalTZA() : double;
        private native function DaylightSavingsTA(t : double) : double;
    }
}
