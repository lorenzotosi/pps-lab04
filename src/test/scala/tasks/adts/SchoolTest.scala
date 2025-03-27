package tasks.adts

import org.junit.Assert.*
import org.junit.Test
import tasks.adts.SchoolModel.*
import tasks.adts.SchoolModel.BasicSchoolModule.*
import u03.extensionmethods.Sequences.Sequence.*
import u03.extensionmethods.Sequences.*


class SchoolTest:
  @Test def testEmptySchool(): Unit =
    val school = emptySchool
    assertEquals(Nil(), school.teachers)
    assertEquals(Nil(), school.courses)
    assertFalse(school.hasTeacher("John"))
    assertFalse(school.hasCourse("Math"))

  @Test def testSetTeacherToCourse(): Unit =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math)
    assertEquals(Cons("John", Nil()), school2.teachers)
    assertEquals(Cons("Math", Nil()), school2.courses)
    assertTrue(school2.hasTeacher("John"))
    assertTrue(school2.hasCourse("Math"))

  @Test def testCoursesOfATeacher(): Unit =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val italian = course("Italian")
    val school2 = school.setTeacherToCourse(john, math).setTeacherToCourse(john, italian)
    assertEquals(Cons("Math", Cons("Italian", Nil())), school2.coursesOfATeacher(john))

  @Test def testDistinctCoursesAndTeachers(): Unit =
    val school = emptySchool
    val john = teacher("John")
    val math = course("Math")
    val school2 = school.setTeacherToCourse(john, math).setTeacherToCourse(john, math)
    assertEquals(Cons("John", Nil()), school2.teachers)
    assertEquals(Cons("Math", Nil()), school2.courses)