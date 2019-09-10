'use strict';

// const FONTS_FILES = "./node_modules/font-awesome/fonts/*.*";
// const SCSS_FILES = "./Resources/sass/*.scss";
const SCRIPTS_FILES = "./Resources/scripts/*.js";
const TYPESCRIPT_FILES = 'assets/typescript/**/*.ts';
const VENDORS_SCRIPTS_FILES = [
    './node_modules/jquery/dist/jquery.js',
    './node_modules/bootstrap/dist/js/bootstrap.min.js'
];

var gulp = require('gulp');
var concat = require("gulp-concat");
var ts = require('gulp-typescript');
var uglify = require("gulp-uglify");

/*
gulp.task("fonts",
    function() {
        return gulp.src(FONTS_FILES)
            .pipe(gulp.dest("./Assets/fonts"));
    });

gulp.task("sass", function () {
    return gulp.src(SCSS_FILES)
      .pipe(sass({ outputStyle: "compressed" }).on("error", sass.logError))
      .pipe(gulp.dest("./Assets/css"));
});
*/

gulp.task("scripts", function () {
    return gulp.src(TYPESCRIPT_FILES)
        .pipe(ts())
        .pipe(gulp.dest("./static/scripts"));
});

gulp.task("vendors:scripts", function () {
    return gulp.src(VENDORS_SCRIPTS_FILES)
        .pipe(concat("vendors.js"))
        .pipe(uglify())
        .pipe(gulp.dest("./static/scripts"));
});

gulp.task('default', gulp.series('scripts', 'vendors:scripts'));

gulp.task("watch", function () {
    // gulp.watch(SCSS_FILES, gulp.series("sass"));
    gulp.watch(TYPESCRIPT_FILES, gulp.series("scripts"));
});