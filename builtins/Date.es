/* -*- mode: java; indent-tabs-mode: nil -*- 
 *
 * ECMAScript 4 builtins - the "date" object
 *
 * E262-3 15.9
 * E262-4 proposals:date_and_time
 *
 * Status: complete; not reviewed; not tested.
 *
 * The Emacs java formatting mode often fails here because of the
 * extensive use of expression functions.
 */

package
{
    use default namespace public;
    use namespace intrinsic;  // Override with "public::" when necessary

    /* The Date class is "final dynamic" in ActionScript 3.0, though
       the motivation for that is unclear.  The consequence is anyway
       that the getters for the components of a Date can be
       inlined.  */

    final dynamic class Date
    {       
        /* E262-3 15.9.2: The Date Constructor Called as a Function */
        static meta function invoke(...args)   // args are ignored.
            (new Date()).public::toString();

        /* E262-3 15.9.3: The Date Constructor. 
         *
         * Plus spidermonkey extension: we accept 0 args (interpreted as "now") or 
         * 1 arg (interpreted as a timespec).
         */
        public function Date(year:double=0, month:double=0, date:double=1, 
                             hours:double=0, minutes:double=0, seconds:double=0, ms:double=0) {

            setupNanoAge();

            switch (arguments.length) {
            case 0:
                timeval = Date.now();
                return;
            case 1:
                let v = ToPrimitive(year);
                if (v is string)
                    return parse(v);

                timeval = TimeClip(ToDouble(v));
                return;
            default:
                ms = ToDouble(ms);
            case 6:
                seconds = ToDouble(seconds);
            case 5:
                minutes = ToDouble(minutes);
            case 4:
                hours = ToDouble(hours);
            case 3:
                date = ToDouble(date);
            case 2:
                year = ToDouble(year);
                month = ToDouble(month);

                /*
                print("Date() :",
                      " year=", year, 
                      ", month=", month,
                      ", date=", date,
                      ", hours=", hours,
                      ", minutes=", minutes,
                      ", seconds=", seconds,
                      ", ms=", ms);
                */

                let intYear : int = ToInteger(year);
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
        public static var parse = function parse(string, reference:double=0.0) {
            if (arguments.length > 2)
                throw new TypeError("Too many arguments to Date.parse"); 
            return Date.parse(ToString(string), reference);
        }

        static intrinsic function parse(s:string, reference:double=0.0) : Date! {

            function fractionToMilliseconds(frac : string) : double
                Math.floor(1000 * (parseInt(frac) / Math.pow(10,frac.length)));
            let isoRes : Object = isoTimestamp.exec(s);
            let defaults : Date! = new Date(reference);
            if (isoRes !== undefined) {
                let year = isoRes.year !== undefined ? parseInt(isoRes.year) : defaults.UTCYear;
                let month = isoRes.month !== undefined ? parseInt(isoRes.month) : defaults.UTCMonth;
                let day = isoRes.day !== undefined ? parseInt(isoRes.day) : defaults.UTCDay;
                let hour = isoRes.hour !== undefined ? parseInt(isoRes.hour) : defaults.UTCHour;
                let mins = isoRes.mins !== undefined ? parseInt(isoRes.mins) : defaults.UTCMinutes;
                let secs = isoRes.secs !== undefined ? parseInt(isoRes.secs) : defaults.UTCSeconds;
                let millisecs = isoRes.fraction !== undefined ? 
                    fractionToMillisecons(isoRes.fraction) : 
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
        static function fromDateString(s : string, reference : double) : Date {

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
            if (res.tz !== undefined) {
                let hour = parseInt(res.tz[1:3]);
                let min  = parseInt(res.tz[3:5]);
                if (res.tz[0] == '+')
                    t -= (hour*60 + min)*60*1000;
                else 
                    t += (hour*60 + min)*60*1000;
            }
            return new Date(t);
        }

        /* E262-4 proposals:date_and_time - "Current and elapsed times" */
        static intrinsic native function now() : double;

        public static function now() : double
            Date.now();

        /* E262-4 proposals:date_and_time - "Current and elapsed times" */
        prototype function nanoAge() : double
            this.nanoAge();

        /* INFORMATIVE */
        intrinsic function nanoAge() : double
            (Date.now() - birthtime) * 1000000;

        /* INFORMATIVE */
        function setupNanoAge() : void
            this.birthtime = Date.now();

        /* E262-3 15.9.4.3: Date.UTC */
        public static var UTC = 
            function UTC(year, month, date, hours, minutes, seconds, ms)
            let (argc:uint = arguments.length)
                Date.UTC(ToDouble(year), 
                         ToDouble(month), 
                         argc >= 3 ? ToDouble(date) : 1,
                         argc >= 4 ? ToDouble(hours) : 0,
                         argc >= 5 ? ToDouble(minutes) : 0,
                         argc >= 6 ? ToDouble(seconds) : 0,
                         argc >= 7 ? ToDouble(ms) : 0);

        static intrinsic function UTC(year : double, month : double, 
				      date : double=1, hours : double?=0, minutes : double?=0,
				      seconds : double=0, ms : double?=0) : double {
            let intYear:double = ToInteger(year);
            if (!isNaN(year) && 0 <= intYear && intYear <= 99)
                intYear += 1900;
            return TimeClip(MakeDate(MakeDay(intYear, month, date), 
                                     MakeTime(hours, minutes, seconds, ms)));
        }

        static const dayNames = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
        static const monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

        /* Format of string produced by Date.toString, recognized by Date.parse */
        /* e.g., "Fri, 15 Dec 2006 23:45:09 GMT-0800" */

        static const adhocTimestamp : RegExp! = 
            /(?: Mon|Tue|Wed|Thu|Fri|Sat|Sun )\s+
             (?P<day> [0-9]+ )\s+
             (?P<month> Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec )\s+
             (?P<year> -? [0-9]+ )\s+
             (?P<hour> [0-9]{2} ):
             (?P<minute> [0-9]{2} ):
             (?P<second> [0-9]{2} )\s+
             GMT
            (?P<tz> (?: \\+ | - ) [0-9]{4} )?/x;

        /* Format of string produced by Date.toISOString, recognized by Date.parse */
        /* e.g, "2006-12-15T23:45:09.33-08:00" */

        static const isoTimestamp : RegExp! =
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
        prototype function toISOString(this:Date)
            this.toISOString();

        intrinsic function toISOString() : string {

            function years(n : double) : String {
                if (n >= 0 && n <= 9999)
                    return (n+10000).toString().substring(1);
                else
                    return n.toString();
            }

            function fraction(n : int) : string {
                while (n > 0 && n % 10 === 0)
                    n /= 10;
                return n.toString();
            }

            let tz:double = timezoneOffset;
            let atz:double = Math.abs(tz);
            return "" + years(fullYear) + "-" + twoDigit(month) + "-" + twoDigit(day) +
                "T" + twoDigit(hours) + ":" + twoDigit(minutes) + ":" + twoDigit(seconds) +
                "." + fraction(int(milliseconds)) + sign(tz) +
                twoDigit(Math.floor(atz / 60)) + ":" + twoDigit(atz % 60);
        }

        /* E262-3 15.9.5.2: Date.prototype.toString */
        prototype function toString(this:Date)
            this.toString(); 

        /* INFORMATIVE */
        override intrinsic function toString() : string
            /* "Fri, 15 Dec 2006 23:45:09 GMT-0800" */
            let (tz:double = timezoneOffset)
	    let (atz:double = Math.abs(tz))
	    (dayNames[day] + ", " + 
	     twoDigit(date) + " " + 
	     monthNames[month] + " " + 
	     fullYear + " " + 
	     twoDigit(hours) + ":" + 
	     twoDigit(minutes) + ":" + 
	     twoDigit(seconds) + " GMT" + 
	     sign(tz) + twoDigit(Math.floor(atz / 60)) + twoDigit(atz % 60));

        /* E262-3 15.9.5.42: Date.prototype.toUTCString */
        prototype function toUTCString(this:Date)
            this.toUTCString(); 

        /* INFORMATIVE */
        intrinsic function toUTCString() : string
            /* "Sat, 16 Dec 2006 08:06:21 GMT" */
            dayNames[UTCDay] + ", " + 
            twoDigit(UTCDate) + " " + 
            monthNames[UTCMonth] + " " +
            UTCFullYear + " " + 
            twoDigit(UTCHours) + ":" + 
            twoDigit(UTCMinutes) + ":" + 
            twoDigit(UTCSeconds) + " GMT";

        /* E262-3 15.9.5.3: Date.prototype.toDateString */
        prototype function toDateString(this:Date)
            this.toDateString(); 

        /* INFORMATIVE */
        intrinsic function toDateString() : string
            /* "Sat, 16 Dec 2006" */
            dayNames[day] + ", " + 
	    twoDigit(date) + " " + 
	    monthNames[month] + " " + 
	    fullYear;

        /* E262-3 15.9.5.4: Date.prototype.toTimeString */
        prototype function toTimeString(this:Date)
            this.toTimeString(); 

        /* INFORMATIVE */
        intrinsic function toTimeString():string
            /* "00:13:29 GMT-0800" */
            let (tz:double = timezoneOffset)
                let (atz:double = Math.abs(tz))
                    twoDigit(hours) + ":" + twoDigit(minutes) + ":" + twoDigit(seconds) +
                    " GMT" + sign(tz) + twoDigit(Math.floor(atz / 60)) + twoDigit(atz % 60);

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
           inlined. */

        public function get time(this:Date) : double                     getTime();
        public function set time(this:Date, t : double) : double         setTime(t);
        public function get fullYear(this:Date) : double                 getFullYear();
        public function set fullYear(this:Date, t : double) : double     setFullYear(t);
        public function get UTCFullYear(this:Date) : double              getUTCFullYear();
        public function set UTCFullYear(this:Date, t : double) : double  setUTCFullYear(t);
        public function get month(this:Date) : double                    getMonth();
        public function set month(this:Date, t : double) : double        setMonth(t);
        public function get UTCMonth(this:Date) : double                 getUTCMonth();
        public function set UTCMonth(this:Date, t : double) : double     setUTCMonth(t);
        public function get date(this:Date) : double                     getDate();
        public function set date(this:Date, t : double) : double         setDate(t);
        public function get UTCDate(this:Date) : double                  getUTCDate();
        public function set UTCDate(this:Date, t : double) : double      setUTCDate(t);
        public function get day(this:Date) : double                      getDay();
        public function set day(this:Date, t : double) : double          setDay(t);
        public function get UTCDay(this:Date) : double                   getUTCDay();
        public function set UTCDay(this:Date, t : double) : double       setUTCDay(t);
        public function get hours(this:Date) : double                    getHours();
        public function set hours(this:Date, t : double) : double        setHours(t);
        public function get UTCHours(this:Date) : double                 getUTCHours();
        public function set UTCHours(this:Date, t : double) : double     setUTCHours(t);
        public function get minutes(this:Date) : double                  getMinutes();
        public function set minutes(this:Date, t : double) : double      setMinutes(t);
        public function get UTCMinutes(this:Date) : double               getUTCMinutes();
        public function set UTCMinutes(this:Date, t : double) : double   setUTCMinutes(t);
        public function get seconds(this:Date) : double                  getSeconds();
        public function set seconds(this:Date, t : double) : double      setSeconds(t);
        public function get UTCSeconds(this:Date) : double               getUTCSeconds();
        public function set UTCSeconds(this:Date, t : double) : double   setUTCSeconds(t);
        public function get milliseconds(this:Date) : double             getMilliseconds();
        public function set milliseconds(this:Date, t : double) : double setMilliseconds(t);
        public function get UTCMilliseconds(this:Date) : double          getUTCMilliseconds();
        public function set UTCMilliseconds(this:Date, t : double) : double setUTCMilliseconds(t);
        public function get timezoneOffset(this:Date) : double            getTimezoneOffset();


        /* Mandated aliases for the canonical getters and setters */

        /* E262-3 15.9.5.9: Date.prototype.getTime */
        prototype function getTime(this:Date)   
            this.getTime();

        intrinsic function getTime() : double   
            timeval;

        /* E262-3 15.9.5.10: Date.prototype.getFullYear */
        prototype function getFullYear(this:Date)
            this.getFullYear();

        intrinsic function getFullYear() : double
            let (t : double = timeval) 
                isNaN(t) ? t : YearFromTime(LocalTime(t));

        /* E262-3 15.9.5.11: Date.prototype.getUTCFullYear */
        prototype function getUTCFullYear(this:Date)
            this.getUTCFullYear();

        intrinsic function getUTCFullYear() : double
            let (t : double = timeval) 
                isNaN(t) ? t : YearFromTime(t);

        /* E262-3 15.9.5.12: Date.prototype.getMonth */
        prototype function getMonth(this:Date)
            this.getMonth();

        intrinsic function getMonth() : double
            let (t : double = timeval)
                isNaN(t) ? t : MonthFromTime(LocalTime(t));
        
        /* E262-3 15.9.5.13: Date.prototype.getUTCMonth */
        prototype function getUTCMonth(this:Date)
            this.getUTCMonth();

        intrinsic function getUTCMonth() : double
            let (t : double = timeval)
                isNaN(t) ? t : MonthFromTime(t);

        /* E262-3 15.9.5.14: Date.prototype.getDate */
        prototype function getDate(this:Date)
            this.getDate();

        intrinsic function getDate() : double
            let (t : double = timeval)
                isNaN(t) ? t : DateFromTime(LocalTime(t));

        /* E262-3 15.9.5.15: Date.prototype.getUTCDate */
        prototype function getUTCDate(this:Date)
            this.getUTCDate();

        intrinsic function getUTCDate() : double
            let (t : double = timeval)
                isNaN(t) ? t : DateFromTime(t);

        /* E262-3 15.9.5.16: Date.prototype.getDay */
        prototype function getDay(this:Date)
            this.getDay();

        intrinsic function getDay() : double
            let (t : double = timeval)
                isNaN(t) ? t : WeekDay(LocalTime(t));

        /* E262-3 15.9.5.17: Date.prototype.getUTCDay */
        prototype function getUTCDay(this:Date)
            this.getUTCDay();

        intrinsic function getUTCDay() : double
            let (t : double = timeval)
                isNaN(t) ? t : WeekDay(t);

        /* E262-3 15.9.5.18: Date.prototype.getHours */
        prototype function getHours(this:Date)
            this.getHours();

        intrinsic function getHours() : double
            let (t : double = timeval)
                isNaN(t) ? t : HourFromTime(LocalTime(t));

        /* E262-3 15.9.5.19: Date.prototype.getUTCHours */
        prototype function getUTCHours(this:Date)
            this.getUTCHours();

        intrinsic function getUTCHours() : double
            let (t : double = timeval)
                isNaN(t) ? t : HourFromTime(t);

        /* E262-3 15.9.5.20: Date.prototype.getMinutes */
        prototype function getMinutes(this:Date)
            this.getMinutes();

        intrinsic function getMinutes() : double
            let (t : double = timeval)
                isNaN(t) ? t : MinFromTime(LocalTime(t));
        
        /* E262-3 15.9.5.21: Date.prototype.getUTCMinutes */
        prototype function getUTCMinutes(this:Date)
            this.getUTCMinutes();

        intrinsic function getUTCMinutes() : double
            let (t : double = timeval)
                isNaN(t) ? t : MinFromTime(t);

        /* E262-3 15.9.5.22: Date.prototype.getSeconds */
        prototype function getSeconds(this:Date)
            this.getSeconds();

        intrinsic function getSeconds() : double
            let (t : double = timeval)
                isNaN(t) ? t : SecFromTime(LocalTime(t));

        /* E262-3 15.9.5.23: Date.prototype.getUTCSeconds */
        prototype function getUTCSeconds(this:Date)
            this.getUTCSeconds();

        intrinsic function getUTCSeconds() : double
            let (t : double = timeval)
                isNaN(t) ? t : SecFromTime(t);

        /* E262-3 15.9.5.24: Date.prototype.getMilliseconds */
        prototype function getMilliseconds(this:Date)
            this.getMilliseconds();

        intrinsic function getMilliseconds() : double
            let (t : double = timeval)
                isNaN(t) ? t : msFromTime(LocalTime(t));
        
        /* E262-3 15.9.5.25: Date.prototype.getUTCMilliseconds */
        prototype function getUTCMilliseconds(this:Date)
            this.getUTCMilliseconds();

        intrinsic function getUTCMilliseconds() : double
            let (t : double = timeval)
                isNaN(t) ? t : msFromTime(t);
        
        /* E262-3 15.9.5.26: Date.prototype.getTimezoneOffset */
        prototype function getTimezoneOffset(this:Date)
            this.getTimezoneOffset();

        intrinsic function getTimezoneOffset() : double
            let (t : double = timeval) 
                isNaN(t) ? t : (t - LocalTime(t)) / msPerMinute;

        /* E262-3 15.9.5.27: Date.prototype.setTime */
        prototype function setTime(this:Date, t)
            this.setTime(double(t));

        intrinsic function setTime(t:double) : double
            timeval = TimeClip(t);

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
            (n + 100).toString().substring(1,3);

        function sign(n : double)
            n < 0 ? "-" : "+";

        /* Primitives from E262-3 */

        var timeval : double;    // This object's time value
        var birthtime : double;  // INFORMATIVE.  For use by nanoAge

        static const hoursPerDay : double = 24;
        static const minutesPerHour : double = 60;
        static const secondsPerMinute : double = 60;
        static const daysPerYear : double = 365.2425;

        static const msPerSecond : double = 1000;
        static const msPerMinute : double = msPerSecond * secondsPerMinute;
        static const msPerHour : double = msPerMinute * minutesPerHour;
        static const msPerDay : double = msPerHour * hoursPerDay;
        static const msPerYear : double = msPerDay * daysPerYear;

        static const monthOffsets : [double] 
            = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334] : [double];

        function MakeTime(hour:double, min:double, sec:double, ms:double ):double {
            if (!isFinite(hour) || !isFinite(min) || !isFinite(sec) || !isFinite(ms))
                return NaN;

            return ToInteger(hour) * msPerHour +
                ToInteger(min) * msPerMinute +
                ToInteger(sec) * msPerSecond +
                ToInteger(ms);
        }

        function MakeDay(year : double, month : double, date : double) : double {
            if (!isFinite(year) || !isFinite(month) || !isFinite(date))
                return NaN;

            year = ToInteger(year);
            month = ToInteger(month);
            date = ToInteger(date);

            /* INFORMATIVE, the spec is non-operational. */
            year += Math.floor(month / 12);

            month = month % 12;
            if (month < 0)
                month += 12;

            let leap = (DaysInYear(year) == 366);

            let yearday = Math.floor(TimeFromYear(year) / msPerDay);
            let monthday = DayFromMonth(month, leap);

            /*
            print("MakeDay() : ",
                  "year=", year,
                  ", month=", month,
                  ", date=", date,
                  ", yearday=", yearday,
                  ", monthday=", monthday,
                  ", leap=", leap);
            */

            return yearday + monthday + date - 1;
        }

        function MakeDate(day : double, time : double) : double {
            if (!isFinite(day) || !isFinite(time))
                return NaN;

            let res = day * msPerDay + time;

            /*
            print("MakeDate() : ",
                  "day=", day,
                  ", time=", time,
                  ", result=", res);
            */
            
            return res;
        }

        function Day(t : double) : double
            Math.floor(t / msPerDay);

        function TimeWithinDay(t : double) : double 
            t % msPerDay;

        function HourFromTime(t : double) : double {
            let v : double = Math.floor(t / msPerHour) % hoursPerDay;
            if (v < 0)
                return v + hoursPerDay;
            return v;
        }        

        function MinFromTime(t : double) : double {
            let v : double = Math.floor(t / msPerMinute) % minutesPerHour;
            if (v < 0)
                return v + minutesPerHour;
            return v;
        }        

        function SecFromTime(t : double) : double {
            let v : double = Math.floor(t / msPerSecond) % secondsPerMinute;
            if (v < 0)
                return v + secondsPerMinute;
            return v;
        }        

        
        function DaysInYear(y : double) : double {
            if (y % 4 !== 0) return 365;
            if (y % 100 !== 0) return 366;
            if (y % 400 !== 0) return 365;
            return 366;
        }

        function DayFromYear(y : double) : double 
            365 * (y-1970) + Math.floor((y-1969)/4) - Math.floor((y-1901)/100) + Math.floor((y-1601)/400);

        function TimeFromYear(y : double) : double
            msPerDay * DayFromYear(y);

        function InLeapYear(t : double) : double
            (DaysInYear(YearFromTime(t)) == 365) ? 0 : 1;

        function MonthFromTime(t : double) : double {
            let dwy : double = DayWithinYear(t),
                ily : double = InLeapYear(t);
            for ( let i : int=monthOffsets.length-1; i >= 0; i-- ) {
                let monthBegin = monthOffsets[i];                
                if (i >= 2)
                    monthBegin += ily;
                if (dwy >= monthBegin)
                    return i;
            }
            /*NOTREACHED*/
        }

        function DayWithinYear(t : double) : double
            Day(t) - DayFromYear(YearFromTime(t));

        function DayFromMonth(m : double, leap : boolean) : double 
            monthOffsets[m] + (leap && m >= 2 ? 1 : 0);

        function DateFromTime(t : double) : double
            // Days-within-year are 0-based. Dates-within-months
            // are 1-based. Hence the dwy+1 below: day 0 == Jan 1.
            let (dwy : double = DayWithinYear(t),
                 mft : double = MonthFromTime(t),
                 ily : double = InLeapYear(t))
                (dwy+1) - (monthOffsets[mft]) - (mft >= 2 ? ily : 0);

        function WeekDay(t : double) : double {
            let v : double = (Day(t) + 4) % 7;
            if (v < 0)
                return v + 7;
            return v;
        }

        function LocalTime(t : double) : double
            t + LocalTZA() + DaylightSavingsTA(t);

        function UTCTime(t : double) : double
            t - LocalTZA() - DaylightSavingsTA(t - LocalTZA());

        function TimeClip(t : double) : double 
            (!isFinite(t) || Math.abs(t) > 8.64e15) ? NaN : ToInteger(t);

        /* INFORMATIVE */
        function YearFromTime(t : double) : double {
            let y : double = Math.floor(t / msPerYear) + 1970;
            let t2 : double = TimeFromYear(y);
            if (t2 > t) {
                y--;
            } else {
                if (t2 + msPerDay * DaysInYear(y) <= t)
                    y++;
            }
            return y;
        }

        native function LocalTZA() : double;
        native function DaylightSavingsTA(t : double) : double;
    }
}
