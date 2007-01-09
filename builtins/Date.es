/* -*- mode: java; mode: font-lock; tab-width: 4; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "date" object
 *
 * E262-3 15.9
 * E262-4 proposals:date_and_time
 *
 * Status: complete but with FIXMEs; not reviewed; not tested.
 *
 * FIXME: Should define date getters in terms of intrinsic getFoo
 * methods, to mirror the setters -- current structure is due to older
 * confusion.
 *
 * The Emacs java formatting mode fails utterly here because of
 * the extensive use of expression functions.
 */

package
{
    use namespace intrinsic;  // Override with "public::" when necessary

    /* The Date class is "final dynamic" in ActionScript 3.0, though
       the motivation for that is unclear.  The consequence is anyway
       that the getters for the components of a Date can be inlined.
    */
    final dynamic class Date extends Object
    {       
        /* E262-3 15.9.2: The Date Constructor Called as a Function */
        static intrinsic function call(...args)   // args are ignored.
            (new Date()).public::toString();

        /* E262-3 15.9.3: The Date Constructor. */
        public function Date(year, month, a, b, c, d, e) {
            let date=1, hours=0, minutes=0, seconds=0, ms=0;

            switch (arguments.length) {
            case 0:  /* 15.9.3.3 */
                timeval = now();
                return;
            case 1:  /* 15.9.3.2 */
                let v = ToPrimitive(year);
                if (v is String)
                    return parse(v cast String);

                timeval = TimeClip(Number(v));
                return;
            default:  /* 15.9.3.1 */
                ms = Number(e);
            case 6:
                seconds = Number(d);
            case 5:
                minutes = Number(c);
            case 4:
                hours = Number(b);
            case 3:
                date = Number(a);
            case 2:
                year = Number(year);
                month = Number(month);

                let intYear = Integer(year);
                if (!isNaN(year) && 0 <= intYear && intYear <= 99)
                    intYear += 1900;

                timeval = TimeClip(UTCTime(MakeDate(MakeDay(intYear, month, date), 
                                                    MakeTime(hours, minutes, seconds, ms))));
                return;
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
        public static var parse = function parse(string, reference:double=0.0) {
            if (arguments.length > 2)
                throw new TypeError("Too many arguments to Date.parse"); 
            return Date.parse(String(string), reference);
        }

        // FIXME: INTERPRETATION: Is reference localtime or UTC?
        static intrinsic function parse(s:String!, reference:double=0.0) : Date! {

            function fractionToMilliseconds(frac : String!) : double
                Math.floor(1000 * (parseInt(frac) / Math.pow(10,frac.length)));

            let isoRes : Object = isoTimestamp.exec(s);
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
        static function fromDateString(s : String!, reference : double) : Date {

            function findMonth(name) {
                for ( let i:int=0 ; i < monthNames.length ; i++ )
                    if (name === monthNames[i])
                        return i;
                return 0;  // implementation bug if this happens...
            }

            let res = adhocTimestamp.exec(s);
            if (res === null)
                return new Date(reference);
            let t = Date.UTC(parseInt(res.year),
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

        /* E262-4 proposals:date_and_time - "Current and elapsed times" */
        static intrinsic native function now() : double;

        public static function now() : double
            Date.now();

        /* E262-4 proposals:date_and_time - "Current and elapsed times" */
        static intrinsic native function nanoAge() : double;

        public static function nanoAge() : double;
            Date.nanoAge();

        /* E262-3 15.9.4.3: Date.UTC */
        public static var UTC = 
            function UTC(year, month, date, hours, minutes, seconds, ms)
            let (argc:uint = arguments.length)
                Date.UTC(Number(year), 
                         Number(month), 
                         argc >= 3 ? Number(date) : 1,
                         argc >= 4 ? Number(hours) : 0,
                         argc >= 5 ? Number(minutes) : 0,
                         argc >= 6 ? Number(seconds) : 0,
                         argc >= 7 ? Number(ms) : 0);

        static instrinsic function UTC(year : double, month : double, 
                                       date : double=1, hours : double?=0, minutes : double?=0,
                                       seconds : double=0, ms : double?=0) : double {
            let intYear:double = Integer(year);
            if (!isNaN(year) && 0 <= intYear && intYear <= 99)
                intYear += 1900;
            return TimeClip(MakeDate(MakeDay(intYear, month, date), 
                                     MakeTime(hours, minutes, seconds, ms)));
        }

        static const dayNames = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];
        static const monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

        /* Format of string produced by Date.toString, recognized by Date.parse */
        /* e.g., "Fri, 15 Dec 2006 23:45:09 GMT-0800" */
        const adhocTimestamp : RegExp! = 
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
        const isoTimestamp : RegExp! =
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

        /* E262-4 proposals:date_and_time - "ISO Date strings" */
        prototype function toISO(this:Date)
            this.toISO();

        intrinsic function toISO() : String
            // FIXME: milliseconds should be minimal string (suppress trailing zeroes)
            // FIXME: the years should be formatted much more elaborately
            let (tz:double = timezoneOffset)
                let (atz:double = Math.abs(tz))
                    "" + fullYear + "-" + twoDigit(month) + "-" + twoDigit(day) +
                    "T" + twoDigit(hours) + ":" + twoDigit(minutes) + ":" + twoDigit(seconds) +
                    "." + threeDigit(milliseconds) + sign(tz) +
                    twoDigit(Math.floor(atz / 60)) + ":" + twoDigit(atz % 60);

        /* E262-3 15.9.5.2: Date.prototype.toString */
        prototype function toString(this:Date)
            this.toString(); 

        /* INFORMATIVE */
        intrinsic function toString() : String
            /* "Fri, 15 Dec 2006 23:45:09 GMT-0800" */
            let (tz:double = timezoneOffset)
                let (atz:double = Math.abs(tz))
                    dayNames[date] + ", " + twoDigit(day) + " " + monthNames[month] + " " fullYear +
                    " " + twoDigit(hours) + ":" + twoDigit(minutes) + ":" + twoDigit(seconds) +
                    " GMT" + sign(tz) + twoDigit(Math.floor(atz / 60)) + twoDigit(atz % 60);

        /* E262-3 15.9.5.42: Date.prototype.toUTCString */
        prototype function toUTCString(this:Date)
            this.toUTCString(); 

        /* INFORMATIVE */
        intrinsic function toUTCString() : String
            /* "Sat, 16 Dec 2006 08:06:21 GMT" */
            dayNames[UTCdate] + ", " + 
            twoDigit(UTCday) + " " + 
            monthNames[UTCmonth] + " " +
            UTCFullYear + " " + 
            twoDigit(UTCHours) + ":" + 
            twoDigit(UTCMinutes) + ":" + 
            twoDigit(UTCSeconds) + " GMT";

        /* E262-3 15.9.5.3: Date.prototype.toDateString */
        prototype function toDateString(this:Date)
            this.toDateString(); 

        /* INFORMATIVE */
        intrinsic function toDateString() : String
            /* "Sat, 16 Dec 2006" */
            dayNames[date] + ", " + twoDigit(day) + " " + monthNames[month] + " " fullYear;

        /* E262-3 15.9.5.4: Date.prototype.toTimeString */
        prototype function toTimeString(this:Date)
            this.toTimeString(); 

        /* INFORMATIVE */
        intrinsic function toTimeString():String
            /* "00:13:29 GMT-0800" */
            let (tz:double = timezoneOffset)
                let (atz:double = Math.abs(tz))
                    twoDigit(hours) + ":" + twoDigit(minutes) + ":" + twoDigit(seconds) +
                    " GMT" + sign(tz) + twoDigit(Math.floor(atz / 60)) + twoDigit(atz % 60);

        /* E262-3 15.9.5.5: Date.prototype.toLocaleString */
        prototype function toLocaleString(this:Date)
            this.toLocaleString(); 

        /* INFORMATIVE */
        intrinsic function toLocaleString():String
            this.toString();

        /* E262-3 15.9.5.6: Date.prototype.toLocaleDateString */
        prototype function toLocaleDateString(this:Date)
            this.toLocaleDateString(); 

        /* INFORMATIVE */
        intrinsic function toLocaleDateString():String
            this.toDateString();

        /* E262-3 15.9.5.7: Date.prototype.toLocaleTimeString */
        prototype function toLocaleTimeString(this:Date)
            this.toLocaleTimeString(); 

        /* INFORMATIVE */
        intrinsic function toLocaleTimeString():String
            this.toTimeString();

        /* E262-3 15.9.5.8: Date.prototype.valueOf */
        prototype function valueOf(this:Date)
            this.valueOf(); 

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
        public function get time() : double
            timeval;

        /* E262-3 15.9.5.27: Date.prototype.setTime */
        public function set time(t : double) : double
            timeval = TimeClip(t);

        /* E262-3 15.9.5.10: Date.prototype.getFullYear */
        public function get fullYear() : double 
            let (t : double = timeval) 
                isNaN(t) ? t : YearFromTime(LocalTime(t));

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set fullYear(t : double) : double
            setFullYear(t)

        /* E262-3 15.9.5.11: Date.prototype.getUTCFullYear */
        public function get UTCFullYear() : double
            let (t : double = timeval) 
                isNaN(t) ? t : YearFromTime(t);

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set UTCFullYear(t : double) : double
            setUTCFullYear(t)

        /* E262-3 15.9.5.12: Date.prototype.getMonth */
        public function get month() : double
            let (t : double = timeval)
                isNaN(t) ? t : MonthFromTime(LocalTime(t));

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set month(t : double) : double
            setMonth(t)

        /* E262-3 15.9.5.13: Date.prototype.getUTCMonth */
        public function get UTCMonth() : double
            let (t : double = timeval)
                isNaN(t) ? t : MonthFromTime(t);

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set UTCMonth(t : double) : double
            setUTCMonth(t)

        /* E262-3 15.9.5.14: Date.prototype.getDate */
        public function get date() : double 
            let (t : double = timeval)
                isNaN(t) ? t : DateFromTime(LocalTime(t));

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set date(t : double) : double
            setDate(t)

        /* E262-3 15.9.5.15: Date.prototype.getUTCDate */
        public function get UTCDate() : double
            let (t : double = timeval)
                isNaN(t) ? t : DateFromTime(t);

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set UTCDate(t : double) : double
            setUTCDate(t)

        /* E262-3 15.9.5.16: Date.prototype.getDay */
        public function get day() : double
            let (t : double = timeval)
                isNaN(t) ? t : WeekDay(LocalTime(t));

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set day(t : double) : double
            setDay(t)

        /* E262-3 15.9.5.17: Date.prototype.getUTCDay */
        public function get UTCDay() : double 
            let (t : double = timeval)
                isNaN(t) ? t : WeekDay(t);

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set UTCDay(t : double) : double
            setUTCDay(t)

        /* E262-3 15.9.5.18: Date.prototype.getHours */
        public function get hours() : double
            let (t : double = timeval)
                isNaN(t) ? t : HourFromTime(LocalTime(t));

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set hours(t : double) : double
            setHours(t)

        /* E262-3 15.9.5.19: Date.prototype.getUTCHours */
        public function get UTCHours() : double 
            let (t : double = timeval)
                isNaN(t) ? t : HourFromTime(t);

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set UTCHours(t : double) : double
            setUTCHours(t)

        /* E262-3 15.9.5.20: Date.prototype.getMinutes */
        public function get minutes() : double 
            let (t : double = timeval)
                isNaN(t) ? t : MinFromTime(LocalTime(t));

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set minutes(t : double) : double
            setMinutes(t)

        /* E262-3 15.9.5.21: Date.prototype.getUTCMinutes */
        public function get UTCMinutes() : double
            let (t : double = timeval)
                isNaN(t) ? t : MinFromTime(t);

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set UTCMinutes(t : double) : double
            setUTCMinutes(t)

        /* E262-3 15.9.5.22: Date.prototype.getSeconds */
        public function get seconds() : double
            let (t : double = timeval)
                isNaN(t) ? t : SecFromTime(LocalTime(t));

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set seconds(t : double) : double
            setSeconds(t)

        /* E262-3 15.9.5.23: Date.prototype.getUTCSeconds */
        public function get seconds() : double
            let (t : double = timeval)
                isNaN(t) ? t : SecFromTime(t);

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set UTCSeconds(t : double) : double
            setUTCSeconds(t)

        /* E262-3 15.9.5.24: Date.prototype.getMilliseconds */
        public function get milliseconds() : double 
            let (t : double = timeval)
                isNaN(t) ? t : msFromTime(LocalTime(t));

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set milliseconds(t : double) : double
            setMilliseconds(t)

        /* E262-3 15.9.5.25: Date.prototype.getUTCMilliseconds */
        public function get UTCMilliseconds() : double
            let (t : double = timeval)
                isNaN(t) ? t : msFromTime(t);

        /* E262-4 draft proposals:date_and_time - "Property access" */
        public function set UTCMilliseconds(t : double) : double
            setUTCMilliseconds(t)

        /* E262-3 15.9.5.26: Date.prototype.getTimezoneOffset */
        public function get timezoneOffset() : double
            let (t : double = timeval) 
                isNaN(t) ? t : (t - LocalTime(t)) / msPerMinute;


        /* Mandated aliases for the canonical getters and setters */

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
            this.setMilliseconds(double(ms))

        intrinsic function setMilliseconds(ms:double) : double
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(Day(t), MakeTime(HourFromTime(t), 
                                                            MinFromTime(t), 
                                                            SecFromTime(t), 
                                                            ms)));

        // 15.9.5.29 Date.prototype.setUTCMilliseconds (ms)
        prototype function setUTCMilliseconds(this:Date, ms)
            this.setUTCMilliseconds(double(ms));

        intrinsic function setUTCMilliseconds(ms:double) : double
            timeval = let (t : double = timeval)
                          MakeDate(Day(t), MakeTime(HourFromTime(t), 
                                                    MinFromTime(t), 
                                                    SecFromTime(t), 
                                                    ms));

        // 15.9.5.30 Date.prototype.setSeconds (sec [, ms ] )
        prototype function setSeconds(this:Date, sec, ms)
            this.setSeconds(double(sec), double(ms)); 

        intrinsic function setSeconds(sec:double, ms:double = milliseconds) : double
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(Day(t), MakeTime(HourFromTime(t), 
                                                            MinFromTime(t), 
                                                            sec, 
                                                            ms)));

        // 15.9.5.31 Date.prototype.setUTCSeconds (sec [, ms ] )
        prototype function setUTCSeconds(this:Date, sec, ms) 
            this.setUTCSeconds(double(sec), double(ms)); 

        intrinsic function setUTCSeconds(sec:double, ms:double = milliseconds) : double
            timeval = let (t : double = timeval)
                          MakeDate(Day(t), MakeTime(HourFromTime(t), 
                                                    MinFromTime(t), 
                                                    sec, 
                                                    ms));

        // 15.9.5.33 Date.prototype.setMinutes (min [, sec [, ms ] ] )
        prototype function setMinutes(this:Date, min, sec, ms)
            this.setMinutes(double(min), double(sec), double(ms)); 

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
            this.setUTCMinutes(double(min), double(sec), double(ms)); 

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
            this.setHours(double(hour), double(min), double(sec), double(ms));

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
            this.setUTCHours(double(hour), double(min), double(sec), double(ms));

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
            this.setDate(double(date)); 

        intrinsic function setDate(date:double):double
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(MakeDay(YearFromTime(t), MonthFromTime(t), date), 
                                           TimeWithinDay(t)));
            

        // 15.9.5.37 Date.prototype.setUTCDate (date)
        prototype function setUTCDate(this:Date, date)
            this.setUTCDate(double(date)); 

        intrinsic function setUTCDate(date:double):double 
            timeval = let (t : double = timeval)
                          MakeDate(MakeDay(YearFromTime(t), MonthFromTime(t), date), 
                                   TimeWithinDay(t));


        // 15.9.5.38 Date.prototype.setMonth (month [, date ] )
        prototype function setMonth(this:Date, month, date)
            this.setMonth(double(month), double(date)); 

        intrinsic function setMonth(month:double, date:double = date):double
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(MakeDay(YearFromTime(t), month, date), 
                                           TimeWithinDay(t)));

        /* E262-3 15.9.5.39: Date.prototype.setUTCMonth */
        prototype function setUTCMonth(this:Date, month, date)
            this.setUTCMonth(double(month), double(date)); 

        intrinsic function setUTCMonth(month:double, date:double = date):double
            timeval = let (t : double = timeval)
                          MakeDate(MakeDay(YearFromTime(t), month, date), 
                                   TimeWithinDay(t));

        /* E262-3 15.9.5.40: Date.prototype.setFullYear */
        prototype function setFullYear(this:Date, year, month, date)
            this.setFullYear(double(year), double(month), double(date)); 

        intrinsic function setFullYear(year:double, 
                                       month:double = month, 
                                       date:double = date) : double 
            timeval = let (t : double = LocalTime(timeval))
                          UTCTime(MakeDate(MakeDay(year, month, date), 
                                           TimeWithinDay(t)));

        /* E262-3 15.9.5.41: Date.prototype.setUTCFullYear */
        prototype function setUTCFullYear(this:Date, year, month, date)
            this.setUTCFullYear(double(year), double(month), double(date)); 

        intrinsic function setUTCFullYear(year:double, 
                                          month:double = month, 
                                          date:double = date) : double 
            timeval = let (t : double = timeval)
                          MakeDate(MakeDay(year, month, date), 
                                   TimeWithinDay(t));


        /* Utilities */

        function twoDigit(n : double)
            (n + 100).toString().substring(1);

        function threeDigit(n : double)
            (n + 1000).toString().substring(1);

        function sign(n : double)
            n < 0 ? "-" : "+";

        /* Primitives from E262-3 */

        var timeval : double = 0;

        static const msPerDay : double = 86400000;
        static const hoursPerDay : double = 24;
        static const minutesPerHour : double = 60;
        static const secondsPerMinute : double = 60;
        static const msPerSecond : double = 1000;
        static const msPerMinute : double = msPerSecond * secondsPerMinute;
        static const msPerHour : double = msPerMinute * minutesPerHour;
        static const monthOffsets : [double] 
            = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334] : [double];

        function MakeTime(hour:double, min:double, sec:double, ms:double ):double {
            if (!isFinite(hour) || !isFinite(min) || !isFinite(sec) || !isFinite(ms))
                return NaN;

            return Integer(hour) * msPerHour +
                Integer(min) * msPerMinute +
                Integer(sec) * msPerSecond +
                Integer(ms);
        }

        function MakeDay(year : double, month : double, date : double) : double {
            if (!isFinite(year) || !isFinite(month) || !isFinite(date))
                return NaN;

            year = Integer(year);
            month = Integer(month);
            date = Integer(date);
            // FIXME
        }

        function MakeDate(day : double, time : double) : double {
            if (!isFinite(day) || !isFinite(time))
                return NaN;

            return day * msPerDay + time;
        }

        function Day(t : double) : double
            Math.floor(t / msPerDay);

        function TimeWithinDay(t : double) : double
            t % msPerDay;

        function HoursFromTime(t : double) : double
            Math.floor(t / msPerHour) % hoursPerDay;
        
        function DaysInYear(y : double) : double {
            if (y % 4 !== 0) return 365;
            if (y % 100 !== 0) return 366;
            if (y % 400 !== 0) return 365;
            return 366;
        }

        function DayFromYear(y : double) : double 
            365*(y-1970) + Math.floor((y-1969)/4) - Math.floor((y-1901)/100) + Math.floor((y-1601)/400);

        function TimeFromYear(y : double) : double
            msPerDay * DayFromYear(y);

        function InLeapYear(t : double) : double
            DaysInYear(YearFromTime(t)) ? 1 : 0;

        function MonthFromTime(t : double) : double {
            let dwy : double = DayWithinYear(t),
                ily : double = InLeapYear(t);
            for ( let i : int=0 ; i < monthOffsets.length-1 ; i++ )
                if (dwy > monthOffsets[i] + (i >= 2 ? ily : 0) && 
                    dwy < monthOffsets[i+1] + (i+1 >= 2 ? ily : 0))
                    return i;
            /*NOTREACHED*/
        }

        function DayWithinYear(t : double) : double
            Day(t) - DayFromYear(YearFromTime(t));

        function DateFromTime(t : double) : double
            let (dwy : double = DayWithinYear(t),
                 mft : double = MonthFromTime(t),
                 ily : double = InLeapYear(t))
                dwy - (monthOffsets[mft] - 1) - (mft >= 2 ? ily : 0);

        function WeekDay(t : double) : double
            (Day(t) + 4) % 7;

        function LocalTime(t : double) : double
            t + LocalTZA() + DaylightSavingTA(t);

        function UTCTime(t : double) : double
            t - LocalTZA() - DaylighSavingsTA(t - LocalTZA());

        function TimeClip(t : double) : double 
            (!isFinite(t) || Math.abs(t) > 8.64e15) ? NaN : Integer(t);

        /* INFORMATIVE */
        function YearFromTime(t : double) : double {
            let y : double = t / (msPerDay * 365);
            while (TimeFromYear(y) < t)
                y += 1;
            while (TimeFromYear(y) > t)
                y -= 1;
            return y;
        }

        native function LocalTZA() : double;
        native function DaylightSavingsTA(t : double) : double;
    }
}
