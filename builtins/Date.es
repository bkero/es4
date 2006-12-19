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

        // 15.9.3 The Date Constructor
        //
        // 15.9.3.1 new Date (year, month [, date [, hours [, minutes [, seconds [, ms ] ] ] ] ] )
        // 15.9.3.2 new Date (value)
        // 15.9.3.3 new Date ( )
        function Date(year, month, date, hours, minutes, seconds, ms) {
            // conceptually:
            // this.[[Value]] = @todo
        }

        // 15.9.4.2 Date.parse (string)
        var parse = function parse(string) {
            return Date.intrinsic::parse(String(string));
        }

        static native function now() : Number;

        static intrinsic native function now() : Number;

        static intrinsic function parse(string:String!):Date {
            // FIXME
            // This is our chance to write a normative date parser...
        }


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

        static instrinsic function UTC(year : Number, month : Number, 
                                       date : Number=1, hours : Number?=0, minutes : Number?=0,
                                       seconds : Number=0, ms : Number?=0) : Number {
            use namespace intrinsic;

            var intYear = ToInteger(year);
            if (!isNaN(year) && 0 <= intYear && intYear <= 99)
                intYear += 1900;
            return TimeClip(MakeDate(MakeDay(intYear, month, date), 
                                     MakeTime(hours, minutes, seconds, ms)));
        }

        // 15.9.5.2 Date.prototype.toString ( )
        prototype function toString(this:Date) { 
            return this.intrinsic::toString(); 
        }

        intrinsic native function toString() : String;

        // 15.9.5.3 Date.prototype.toDateString ( )
        prototype function toDateString(this:Date) { 
            return this.intrinsic::toDateString(); 
        }

        intrinsic native function toDateString() : String;

        // 15.9.5.4 Date.prototype.toTimeString ( )

        prototype function toTimeString(this:Date) { 
            return this.intrinsic::toTimeString(); 
        }

        intrinsic native function toTimeString():String;

        // 15.9.5.5 Date.prototype.toLocaleString ( )
        prototype function toLocaleString(this:Date) {
            return this.intrinsic::toLocaleString(); 
        }

        intrinsic native function toLocaleString():String;

        // 15.9.5.6 Date.prototype.toLocaleDateString ( )
        prototype function toLocaleDateString(this:Date) {
            return this.intrinsic::toLocaleDateString(); 
        }

        intrinsic native function toLocaleDateString():String;

        // 15.9.5.7 Date.prototype.toLocaleTimeString ( )
        prototype function toLocaleTimeString(this:Date) {
            return this.intrinsic::toLocaleTimeString(); 
        }

        intrinsic native function toLocaleTimeString():String;

        // 15.9.5.42 Date.prototype.toUTCString ( )
        prototype function toUTCString(this:Date) {
            return this.intrinsic::toUTCString(); 
        }
        
        intrinsic native function toUTCString():String;

        // 15.9.5.8 Date.prototype.valueOf ( )
        prototype function valueOf(this:Date) {
            return this.intrinsic::valueOf(); 
        }

        intrinsic native function valueOf():Object;

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

        /*** Informative ***/

        private function YearFromTime(t : double) : double {
            let y : double = t / (msPerDay * 365);
            while (TimeFromYear(y) < t)
                y += 1;
            while (TimeFromYear(y) > t)
                y -= 1;
            return y;
        }

        /*** System hooks ***/

        private native function LocalTZA() : double;
        private native function DaylightSavingsTA(t : double) : double;
    }
}
