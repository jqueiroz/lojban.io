module Server.Api.V0.Serializers
( serializeCourse
) where

import Core
import Language.Lojban.Core
import qualified Server.Api.V0.Contract as Contract

serializeCourse :: Course -> Contract.Course
serializeCourse course = Contract.Course title dictionaryId style where
    title = courseTitle course
    dictionaryId = dictIdentifier (courseDictionary course)
    style = serializeCourseStyle (courseStyle course)

serializeCourseStyle :: CourseStyle -> Contract.CourseStyle
serializeCourseStyle courseStyle = Contract.CourseStyle color1 iconUrl where
    color1 = courseStyleColor1 courseStyle
    iconUrl = courseStyleIconUrl courseStyle
