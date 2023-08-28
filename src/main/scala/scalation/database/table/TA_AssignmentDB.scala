
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Aug 10 15:19:54 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Database: TA Assignments
 */

package scalation
package database
package table

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TA_AssignmentDB` object contains table definitions for making TA Assignments.
 */
object TA_AssignmentDB:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The schedule table - period to class time mapping
     *  period:  the class period
     *  mwf:     times for MWF classes
     *  tr:      times for TR classes
     */
    val schedule = Table ("schedule", "period, mwf, tr",
                                      "I, S, S", "period")

    schedule += (1, "8:00 - 8:50", "8:00 - 9:15")
             += (2, "9:10 - 10:00", "9:35 - 10:50")
             += (3, "10:20 - 11:10", "11:10 - 12:25")
             += (4, "11:30 - 12:20", "12:45 - 2:00")
             += (5, "12:40 - 1:30", "2:20 - 3:35")
             += (6, "1:50 - 2:40", "3:55 - 5:10")
             += (7, "3:00 - 3:50", "5:30 - 6:45")
             += (8, "4:10 - 5:00", "6:30 - 7:45")
             += (9, "5:20 - 6:10", "8:00 - 9:15")
             += (10, "6:30 - 7:20", "9:30 - 10:45")
             += (11, "6:50 - 7:40", "")
             += (12, "7:55 - 8:45", "")
             += (13, "9:00 - 9:50", "")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The course table - for courses to be assigned TAs.
     *  dept:   department code
     *  cno:    course number (lab course 13011 means 1301L)
     *  title:  course title
     */
    val course = Table ("course", "dept, cno, title",
                                  "S, I, X", "cno")

    course += ("CSCI", 1100,  "Topics in Computing")
           += ("CSCI", 11001, "Topics in Computing Lab")
           += ("CSCI", 1210,  "Computer Modeling and")
           += ("CSCI", 1300,  "Intro to Python Programming")
           += ("CSCI", 13001, "Intro Python Programming Lab")
           += ("CSCI", 1301,  "Intro Computing and Program")
           += ("CSCI", 13011, "Intro Computing Program Lab")
           += ("CSCI", 1302,  "Software Development")
           += ("CSCI", 1360,  "Informatics and Data Analytics")
           += ("CSCI", 1730,  "Systems Programming")
           += ("CSCI", 2150,  "Intro Scientif Comp")
           += ("CSCI", 21501, "Lab Scientif Comp")
           += ("CSCI", 2610,  "Discrete Mathematics for CSCI")
           += ("CSCI", 2611,  "Discrete Mathematics for Engr")
           += ("CSCI", 2670,  "Intro to Theory of Computing")
           += ("CSCI", 2720,  "Data Structures")
           += ("CSCI", 2725,  "Data Structures Data Science")
           += ("CSCI", 3030,  "Computing Ethics and Society")
           += ("CSCI", 3360,  "Data Science I")
           += ("CSCI", 4050,  "Software Engineering")
           += ("CSCI", 4060,  "Mobile Software Development")
           += ("CSCI", 4150,  "Num Simu Sci and Eng")
           += ("CSCI", 4170,  "Computational Investing")
           += ("CSCI", 4210,  "Simulation and Modeling")
           += ("CSCI", 4250,  "Cyber Security")
           += ("CSCI", 4260,  "Data Security and Privacy")
           += ("CSCI", 4300,  "Web Programming")
           += ("CSCI", 4360,  "Data Science II")
           += ("CSCI", 4370,  "Database Management")
           += ("CSCI", 4380,  "Data Mining")
           += ("CSCI", 4470,  "Algorithms")
           += ("CSCI", 4530,  "Introduction to Robotics")
           += ("CSCI", 4550,  "Artif Intelligence")
           += ("CSCI", 4560,  "Evolutionary Computation Apps")
           += ("CSCI", 4600,  "Reinforcement Learning")
           += ("CSCI", 4670,  "Combinatorics")
           += ("CSCI", 4690,  "Graph Theory")
           += ("CSCI", 4720,  "Computer Architecture Organiza")
           += ("CSCI", 4730,  "Operating Systems")
           += ("CSCI", 4760,  "Computer Networks")
           += ("CSCI", 4780,  "Distr Comp Systems")
           += ("CSCI", 4795,  "Cloud Computing")
           += ("CSCI", 4800,  "Human-Comp Interact")
           += ("CSCI", 4810,  "Computer Graphics")
           += ("CSCI", 4830,  "Virtual Reality")
           += ("CSCI", 4840,  "Signal Processing")
           += ("CSCI", 4850,  "Biomedical Image Analysis")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The section table - for sections of courses to be assigned TAs.
     *  crn:      course-section reference number
     *  cno:      course number
     *  period1:  main time periods
     *  period2:  additional time periods
     *  ilname:   name of the instructor
     *  seats:    maximum enrollment
     *  avail:    seats still available
     */
    val section = Table ("section", "crn, cno, period1, period2, ilname, seats, avail, total",
                         "I, I, X, X, S, I, I, I", "crn")

    section += (14820, 1100,  "M W 01:50 pm-02:40 pm", "", "Vance", 84, 64, 20)
            += (36802, 11001, "M W 12:40 pm-01:30 pm", "", "Vance", 24, 7,  17)

    section += (47208, 1300,  "T R 02:20 pm-03:35 pm", "", "Wang", 48, 14, 34)
            += (47210, 13001, "M W 11:30 am-12:20 pm", "", "Wang", 24, 7,  17)
            += (52888, 13001, "M W 09:10 am-10:00 am", "", "Wang", 24, 7,  17)

    section += (15032, 1301,  "T R 09:35 am-10:50 am", "", "LaMarca",  120, 11, 109)
            += (15033, 1301,  "T R 11:10 am-12:25 pm", "", "Liang",    102, 0,  102)
            += (16242, 1301,  "T R 12:45 pm-02:00 pm", "", "LaMarca",  102, 2,  100)
            += (16260, 1301,  "T R 08:00 am-09:15 am", "", "Liang",     96, 2,   94)
            += (55008, 1301,  "T R 02:20 pm-03:35 pm", "", "Liang",    100, 0,  100)
            += (55911, 1301,  "T R 03:55 pm-05:10 pm", "", "Keshtgari", 80, 22,  58)
            += (19917, 13011, "M W 03:00 pm-03:50 pm", "", "LaMarca",   30, 2,   28)
            += (19921, 13011, "T R 12:45 pm-02:00 pm", "", "LaMarca",   30, 0,   30)
            += (19928, 13011, "T R 03:55 pm-05:10 pm", "", "LaMarca",   30, 1,   29)
            += (19935, 13011, "T R 05:30 pm-06:45 pm", "", "LaMarca",   30, 9,   21)
            += (19944, 13011, "M W 09:10 am-10:00 am", "", "Liang",     24, 0,   24)
            += (19954, 13011, "M W 12:40 pm-01:30 pm", "", "Liang",     24, 0,   24)
            += (19957, 13011, "M W 03:00 pm-03:50 pm", "", "Liang",     24, 0,   24)
            += (19959, 13011, "T R 03:55 pm-05:10 pm", "", "Liang",     30, 0,   30)
            += (19960, 13011, "M W 01:50 pm-02:40 pm", "", "LaMarca",   24, 0,   24)
            += (19963, 13011, "M W 04:10 pm-05:00 pm", "", "LaMarca",   24, 1,   23)
            += (19964, 13011, "T R 09:35 am-10:50 am", "", "LaMarca",   24, 1,   23)
            += (22855, 13011, "T R 02:20 pm-03:35 pm", "", "LaMarca",   30, 0,   30)
            += (36715, 13011, "M W 09:10 am-10:00 am", "", "Liang",     24, 0,   24)
            += (36719, 13011, "M W 11:30 am-12:20 pm", "", "Liang",     24, 0,   24)
            += (38021, 13011, "M W 10:20 am-11:10 am", "", "Liang",     24, 3,   21)
            += (38023, 13011, "T R 11:10 am-12:25 pm", "", "Liang",     24, 0,   24)
            += (55029, 13011, "T R 12:45 pm-02:00 pm", "", "Liang",     24, 0,   24)
            += (55030, 13011, "T R 03:55 pm-05:10 pm", "", "Liang",     24, 0,   24)
            += (55031, 13011, "T R 11:10 am-12:25 pm", "", "Liang",     30, 0,   30)
            += (55032, 13011, "M W 04:10 pm-05:00 pm", "", "Liang",     30, 8,   22)
            += (55916, 13011, "T R 08:00 am-09:15 am", "", "Keshtgari", 20, 0,   20)
            += (55918, 13011, "M W 08:00 am-08:50 am", "", "Keshtgari", 20, 1,   19)
            += (55919, 13011, "T R 08:00 am-09:15 am", "", "Keshtgari", 20, 0,   20)

    section += (16468, 1302, "T R 09:35 am-10:50 am", "M 10:20 am-11:10 am",   "Cotterell", 85, 0, 85)
            += (16471, 1302, "M 11:30 am-12:20 pm",   "T R 11:10 am-12:25 pm", "Cotterell", 90, 0, 90)
            += (25528, 1302, "T R 02:20 pm-03:35 pm", "M 03:00 pm-03:50 pm",   "Barnes",    85, 0, 85)
            += (40417, 1302, "T R 03:55 pm-05:10 pm", "M 04:10 pm-05:00 pm",   "Barnes",    48, 0, 48)
            += (55378, 1302, "M 08:00 am-08:50 am",   "T R 08:00 am-09:15 am", "Cotterell", 48, 0, 48)

    section += (16625, 1730, "T R 03:55 pm-05:10 pm", "W 12:40 pm-01:30 pm",   "Peng", 30, 0, 30)    // Lecture 3:55
            += (16632, 1730, "W 01:50 pm-02:40 pm",   "T R 03:55 pm-05:10 pm", "Peng", 30, 0, 30)
            += (16636, 1730, "T R 03:55 pm-05:10 pm", "T 09:35 am-10:50 am",   "Peng", 30, 0, 30)
            += (16641, 1730, "R 09:35 am-10:50 am",   "T R 03:55 pm-05:10 pm", "Peng", 30, 0, 30)
            += (30587, 1730, "T R 05:30 pm-06:45 pm", "W 10:20 am-11:10 am",   "Peng", 30, 0, 30)    // lecture 5:30
            += (30588, 1730, "W 11:30 am-12:20 pm",   "T R 05:30 pm-06:45 pm", "Peng", 30, 0, 30)
            += (30589, 1730, "T 02:20 pm-03:35 pm",   "T R 05:30 pm-06:45 pm", "Peng", 30, 0, 30)
            += (52890, 1730, "T R 05:30 pm-06:45 pm", "R 02:20 pm-03:35 pm",   "Peng", 30, 0, 30)

    section += (22426, 2150,  "T R 12:45 pm-02:00 pm", "", "Hollingsworth", 60, 0,  60)
            += (49678, 2150,  "T R 03:55 pm-05:10 pm", "", "LaMarca",       60, 44, 16)
            += (22826, 21501, "M W 01:50 pm-02:40 pm", "", "Hollingsworth", 30, 0,  30)
            += (30577, 21501, "M W 05:20 pm-06:10 pm", "", "Hollingsworth", 30, 0,  30)
            += (49679, 21501, "M W 10:20 am-11:10 am", "", "LaMarca",       30, 22,  8)
            += (49680, 21501, "M W 04:10 pm-05:00 pm", "", "LaMarca",       30, 22,  8)

    section += (15151, 2610, "T R 05:30 pm-06:45 pm",   "M 05:20 pm-06:10 pm",     "Amirian",  90, 0, 90)
            += (15152, 2610, "T R 12:45 pm-02:00 pm",   "M 12:40 pm-01:30 pm",     "Meena",    85, 0, 85)
            += (25646, 2610, "M 04:10 pm-05:00 pm",     "T R 03:55 pm-05:10 pm",   "Amirian",  85, 0, 85)
            += (55319, 2610, "T 11:10 am-12:25 pm",     "M W F 10:20 am-11:10 am", "Berglund", 28, 0, 28)
            += (55320, 2610, "M W F 11:30 am-12:20 pm", "T 12:45 pm-02:00 pm",     "Berglund", 28, 0, 28)

    section += (15153, 2670, "M 10:20 am-11:10 am",   "T R 09:35 am-10:50 am", "Hollingsworth", 70, 0, 70)
            += (22423, 2670, "T R 11:10 am-12:25 pm", "M 11:30 am-12:20 pm",   "Cai",           70, 0, 70)
            += (32574, 2670, "T R 03:55 pm-05:10 pm", "M 04:10 pm-05:00 pm",   "Hollingsworth", 48, 0, 48)

    section += (17504, 2720, "T R 09:35 am-10:50 am", "M 10:20 am-11:10 am",   "Meena",   75, 0, 75)
            += (30839, 2720, "M 12:40 pm-01:30 pm",   "T R 12:45 pm-02:00 pm", "Amirian", 48, 0, 48)
            += (42634, 2720, "M 04:10 pm-05:00 pm",   "T R 03:55 pm-05:10 pm", "Meena",   75, 0, 75)

    section += (15155, 3030, "M 10:20 am-11:10 am",   "M W 08:00 am-08:50 am", "Funk",     30, 0, 30)     // lecture 8:00
            += (25553, 3030, "M 11:30 am-12:20 pm",   "M W 08:00 am-08:50 am", "Funk",     30, 0, 30)
            += (28723, 3030, "M 12:40 pm-01:30 pm",   "M W 08:00 am-08:50 am", "Funk",     30, 1, 29)
            += (34115, 3030, "M W 08:00 am-08:50 am", "M 01:50 pm-02:40 pm",   "Funk",     30, 9, 21)
            += (49688, 3030, "M W 12:40 pm-01:30 pm", "F 12:40 pm-01:30 pm",   "Stephens", 30, 0, 30)     // lecture 12:40
            += (49689, 3030, "M W 12:40 pm-01:30 pm", "F 11:30 am-12:20 pm",   "Stephens", 30, 0, 30)
            += (49690, 3030, "M W 12:40 pm-01:30 pm", "F 10:20 am-11:10 am",   "Stephens", 30, 0, 30)
            += (49691, 3030, "F 09:10 am-10:00 am",   "M W 12:40 pm-01:30 pm", "Stephens", 30, 0, 30)

    section += (37808, 3360, "T R 12:45 pm-02:00 pm", "W 12:40 pm-01:30 pm", "Peng", 65, 0, 65)

    section += (15157, 4050, "T R 09:35 am-10:50 am", "M 10:20 am-11:10 am",   "Saleh", 19, 4, 42)     // 29, 2
            += (33828, 4050, "T R 12:45 pm-02:00 pm", "M 12:40 pm-01:30 pm",   "Saleh", 30, 1, 29)
            += (35625, 4050, "M 11:30 am-12:20 pm",   "T R 11:10 am-12:25 pm", "Saleh", 40, 1, 39)

    section += (39416, 4060, "W 10:20 am-11:10 am", "T R 09:35 am-10:50 am", "Kochut", 40, 0, 45)      // 8, 3

    section += (52977, 4170, "W 11:30 am-12:20 pm", "T R 11:10 am-12:25 pm", "Hybinette", 50, 14, 47)  // 15, 4

    section += (40571, 4250, "M 10:20 am-11:10 am", "T R 09:35 am-10:50 am", "Khandaker", 37, 3, 39)   // 5, 0

    section += (45971, 4260, "T R 09:35 am-10:50 am", "M 10:20 am-11:10 am", "J. Lee", 32, 5, 40)      // 16, 3

    section += (31786, 4300, "T R 02:20 pm-03:35 pm", "M 03:00 pm-03:50 pm",   "Stephens", 54, 0, 54)
            += (44457, 4300, "M 04:10 pm-05:00 pm",   "T R 03:55 pm-05:10 pm", "Stephens", 60, 8, 60)

    section += (41968, 4360, "M 03:00 pm-03:50 pm", "T R 02:20 pm-03:35 pm", "N. Liu", 20, 2, 55)      // 40, 3

    section += (37931, 4370, "M 04:10 pm-05:00 pm", "T R 03:55 pm-05:10 pm", "Arpinar", 35, 2, 67)     // 35, 1

    section += (33842, 4380, "W 12:40 pm-01:30 pm", "T R 12:45 pm-02:00 pm", "Van Orman", 24, 12, 31)  // 24, 5
            += (52979, 4380, "W 04:10 pm-05:00 pm", "T R 03:55 pm-05:10 pm", "Van Orman", 25, 5,  33)  // 23, 10

    section += (31873, 4470, "T R 12:45 pm-02:00 pm", "M 12:40 pm-01:30 pm", "Funk", 19, 6, 68)        // 55, 0

    section += (40326, 4530, "M 11:30 am-12:20 pm", "T R 11:10 am-12:25 pm", "N. Parasuraman", 15, 1, 22)  // 15, 7
                                                                                            // 15, 5, 18)  // 15, 7

    section += (37840, 4550, "T R 09:35 am-10:50 am", "", "Van Orman", 15, 2, 21)                       // 10, 2
            += (47276, 4550, "T R 03:55 pm-05:10 pm", "", "Maier",     15, 0, 25)                       // 10, 0

    section += (37885, 4560, "M 03:00 pm-03:50 pm", "T R 02:20 pm-03:35 pm", "Rasheed", 30, 1, 44)      // 18, 3

    section += (31806, 4720, "M 11:30 am-12:20 pm",   "T R 11:10 am-12:25 pm", "Keshtgari", 70, 1, 69)
            += (39857, 4720, "T R 02:20 pm-03:35 pm", "M 03:00 pm-03:50 pm",   "Keshtgari", 65, 5, 60)

    section += (16853, 4730, "W 05:20 pm-06:10 pm", "T R 05:30 pm-06:45 pm", "K. Lee", 37, 3, 56)       // 22, 0

    section += (31872, 4760, "T R 08:00 am-09:15 am", "M 08:00 am-08:50 am", "Keshtgari", 65, 0, 65)

    section += (38585, 4800, "M 03:00 pm-03:50 pm", "T R 02:20 pm-03:35 pm", "Cotterell", 40, 0, 44)    // 5, 1

    section += (52937, 4810, "M 03:00 pm-03:50 pm", "T R 02:20 pm-03:35 pm", "Arabnia", 45, 0, 48)      // 3, 0

    section += (53061, 4830, "T R 12:45 pm-02:00 pm", "W 12:40 pm-01:30 pm", "Johnsen", 37, 0, 43)      // 8, 2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The section2 table - for sections of courses taken by TAs.
     *  crn:      course-section reference number
     *  cno:      course number
     *  period1:  main time periods
     *  period2:  additional time periods
     *  ilname:   name of the instructor
     *  seats:    maximum enrollment
     *  avail:    seats still available
     */
    val section2 = Table ("section2", "crn, cno, period1, period2, ilname, seats, avail, total",
                          "I, I, X, X, S, I, I, I", "crn")

    section2 += (15817, 6670, "M W F 03:00 pm-03:50 pm", "", "Chastkofsky", 5, 5, 0)
             += (41970, 6720, "T R 11:10 am-12:25 pm", "W 11:30 am-12:20 pm", "Bhandarkar", 48, 14, 34)
             += (41971, 6760, "W 03:00 pm-03:50 pm", "T R 02:20 pm-03:35 pm", "Perdisci", 35, 0, 35)
             += (54070, 6900, "T R 02:20 pm-03:35 pm", "W 03:00 pm-03:50 pm", "Sun", 23, 16, 7)

    section2 += (55838, 8250, "T R 03:55 pm-05:10 pm", "W 04:10 pm-05:00 pm", "Wang", 20, 4, 16)
             += (52909, 8370, "T R 03:55 pm-05:10 pm", "W 04:10 pm-05:00 pm", "Miller", 30, 0, 30)
             += (52918, 8790, "T R 12:45 pm-02:00 pm", "W 12:40 pm-01:30 pm", "Ramaswamy", 30, 0, 30)
             += (45976, 8795, "T R 02:20 pm-03:35 pm", "W 03:00 pm-03:50 pm", "Kim", 25, 0, 25)
             += (52919, 8850, "W 11:30 am-12:20 pm", "T R 11:10 am-12:25 pm", "T. Liu", 45, 0, 45)
             += (41973, 8945, "T R 12:45 pm-02:00 pm", "W 12:40 pm-01:30 pm", "Miao", 35, 0, 35)
             += (41978, 8965, "W 10:20 am-11:10 am", "T R 09:35 am-10:50 am", "Guan", 22, 1, 21)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The teaching assistants table.
     *  lname:  last name of TA
     *  fname:  first name of TA
     *  level:  MS 0, PhD 1, IoR 2, GSRA 3
     *  labok:  lab TA needs MS or 18 hours of graduate coursework (-1 => unknown, 0 => no, 1 => yes)
     *  tep:    English Level 3, Level 4
     *  hours:  number of hours per week, e.g., 13.3 or 17.8
     */
    val ta = Table ("ta", "email, lname, fname, level, labok, tep, hours",
                          "X, S, S, I, I, I, D", "lname")

        ta += ("in@oxford.edu.uk", "Newton",   "Issac",  0, 0, 3, 13.3)
           += ("ae@princeton.edu", "Einstein", "Albert", 1, 1, 4, 17.8)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The class schdedule for the teaching assistants table.
     *  lname:    last name of TA
     *  periods:  the perioids for which the TA is taking classes
     *            71,72,73,74,75,76,77,78,,1,2,3,4,5,6,7,8,9,10
     *  cnos:     the course numbers as string, e.g., "6370, 6470"
     */
    val ta_sched = Table ("ta_sched", "lname, fname, periods, cnos",
                          "S, S, X, X", "lname")

        ta_sched += ("Newton", "Issac", ",,,,TR,TR,,,,,,,M,M,,M,MW,MW,,,", "6370,6360,8990,GRSC7001,GRSC7770")
                 += ("Einstein", "Albert", ",,,,,,,,,,,,,,,,,,,,", "9000")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The preferred teaching assistants per faculty member x course.
     *  ilname:   instructor name
     *  cno:      course number
     *  lname1:   last name of TA - first choice
     *  lname2:   last name of TA - second choice
     */
    val i_pref = Table ("i_pref", "ilname, cno, lname1, lname2",
                        "S, I, S, S", "ilname")

        i_pref += ("Amirian", 2610, "Newton", "Einstein")
               += ("Arabnia", 4810, "Newton", "")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The assignment of teaching assistants to courses (cno) / sections (crn) table.
     *  lname:   last name of TA
     *  fname:   first name of TA
     *  cno:     course name
     *  crn:     course-section reference number (-1 => delegated to instructor)
     *  hours:   hours per week assigned the instructor-course
     *  ilname:  instructor name
     */
    val ta_assign = Table ("ta_assign", "lname, cno, ilname, hours",
                           "S, I, S, D", "lname")

        ta_assign += ("Einstein", 2610, "Amirian", 17.78)

        ta_assign += ("Newton", 4810, "Arabnia", 13.33)

end TA_AssignmentDB

import TA_AssignmentDB._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `showTabs` main function prints all the tables defined in the `TA_AssignmentDB` object.
 *  > runMain scalation.database.table.showTabs
 */
@main def showTabs (): Unit =

    banner ("course")
    course.show ()

    banner ("section")
    section.show ()

    banner ("section2")
    section2.show ()

    banner ("ta")
    ta.show ()

    banner ("ta_sched")
    ta_sched.show ()

    banner ("i_pref")
    i_pref.show ()

    banner ("ta_assign")
    ta_assign.show ()

    banner ("schedule")
    schedule.show ()

    banner ("instructors")
    val instructors = section.project ("ilname").orderBy ("ilname")
    instructors.create_index ()
    instructors.show ()

    banner ("no_pref")
    val with_pref = i_pref.project ("ilname")
    val no_pref = instructors - with_pref
    no_pref.show ()

    banner ("ta_lname")
    val ta_lname = ta.project ("lname")
    ta_lname.show ()

    banner ("tax_lname")
    val tax_lname = ta_lname - ta_sched.project ("lname")
    tax_lname.show ()

    banner ("ta_4000")
    val ta_4000 = ta_assign.select ("cno > 4000")
//  val ta_4000 = ta_assign.select (t => t(ta_assign.on("cno")) > 4000).orderBy ("cno")
    ta_4000.show ()

    banner ("missing_ta")
    val cnos = section.project ("cno")
    cnos.create_index ()
    val missing_ta = cnos - ta_assign.project ("cno")
    missing_ta.show ()

    banner ("small")
    val small = section.select (t => t(section.on("total")) < 30).orderBy ("cno")
    small.show ()

    banner ("section_ta")
    val section_ta = section leftJoin ta_assign
    section_ta.show ()
    section_ta.writeCSV ("section_ta.csv")

    val periods = ta_sched.toVectorS (2)
    println (s"periods = $periods")
    for i <- periods.indices do println (s"$i: \t ${periods(i)}")
    for i <- periods.indices do println (s"$i: \t ${periods(i).count (_ == ',')}")

    ta_assign.writeCSV ()

end showTabs

