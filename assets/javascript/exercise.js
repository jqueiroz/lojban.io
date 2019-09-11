var createExercisesManager = function(holder) {
    // Elements
    var body = null;
    var footer = null;
    var resetElements = function() {
        holder.empty();
        holder.attr("class", "exercise-holder");
        body = $("<div/>").addClass("exercise-body");
        footer = $("<div/>").addClass("exercise-footer");
        holder.append(body);
        holder.append(footer);
    };
    // Key map
    var keyMap = (function() {
        var map = {};
        $("body").keydown(function(e) {
            var callback = map[e.keyCode];
            if (callback) {
                callback();
                return false;
            }
        });
        return {
            clear: function() {
                map = {};
            },
            clearNumber: function() {
                for (var num = 0; num <= 9; ++num) delete map[48 + num];
            },
            number: function(num, callback) {
                if (num < 0 || num > 9) throw new Error("Illegal argument.");
                map[48 + num] = callback;
                map[96 + num] = callback;
            },
            next: function(callback) {
                map[32] = callback;
                map[39] = callback;
                map[40] = callback;
                map[74] = callback;
                map[76] = callback;
            },
            previous: function(callback) {
                map[37] = callback;
                map[38] = callback;
                map[75] = callback;
                map[72] = callback;
            },
            enter: function(callback) {
                map[13] = callback;
            },
        };
    })();
    // Requests
    var retrieve = function() {
        return $.ajax({
            url: exercise_id + "/get",
            method: "GET",
            dataType: "json",
        });
    };

    var submit = function(data) {
        return $.ajax({
            url: exercise_id + "/submit",
            method: "POST",
            dataType: "json",
            data: JSON.stringify(data),
        });
    };
    // ...
    var exercise_id = null;

    var nextExercise = function() {
        exercise_id = Math.floor(Math.random() * 1000);
        //exercise_id = 485;
        show();
    };

    var show = function() {
        console.log("Showing exercise: " + exercise_id);
        keyMap.clear();

        retrieve(exercise_id).done(function(response) {
            resetElements();
            displayExercise(response);
        });
    };
    // UI
    var displayFeedbackFooter = function(data) {
        keyMap.clear();
        footer.empty();
        footer.addClass(data.correct ? "correct" : "incorrect");
        // Contents
        var contents = $("<div/>").addClass("contents");
        footer.append(contents);
        // Title
        var title = $("<span/>")
            .addClass("title")
            .text(data.correct ? "You are correct" : "You made a mistake");
        contents.append(title);
        // Message
        if (data.correct && data.alternativeAnswer) {
            var alternative_answer = $("<p/>")
                .addClass("answer")
                .text("Alternative answer: " + data.alternativeAnswer);
            contents.append(alternative_answer);
        } else if (!data.correct && data.correctAnswer) {
            var correct_answer = $("<p/>")
                .addClass("answer")
                .text("Correct answer: " + data.correctAnswer);
            contents.append(correct_answer);
        }
        // Button
        var btnContinue = $("<button/>")
            .addClass("success")
            .text("Continue");
        footer.append(btnContinue);
        btnContinue.focus();
        // Events
        btnContinue.click(nextExercise);
        keyMap.enter(nextExercise);
        // Sound
        if (data.correct) {
            new Audio("/static/audio/correct.mp3").play();
        } else {
            new Audio("/static/audio/incorrect.mp3").play();
        }
    };

    var displayExercise = function(data) {
        // Footer
        footer.empty();
        var btnSkip = $("<button/>")
            .addClass("skip")
            .text("Skip");
        footer.append(btnSkip);
        var btnCheck = $("<button/>")
            .addClass("success")
            .text("Check");
        footer.append(btnCheck);
        btnSkip.click(nextExercise);
        // Body
        body.empty();
        if (data.type === "single-choice") {
            // Title
            var title = $("<div/>")
                .addClass("title")
                .html(data.title);
            body.append(title);
            // Table
            var table = $("<div/>").addClass("table");
            body.append(table);
            // Sentences
            for (let i = 0; i < data.sentences.length; ++i) {
                let sentence = data.sentences[i];
                let sentenceElement = $("<div/>")
                    .addClass("sentence")
                    .html(sentence.text);
                if (sentence.lojbanic) {
                    sentenceElement.addClass("lojbanic");
                }
                if (data.sentences.length === 1) {
                    sentenceElement.addClass("single");
                }
                table.append(sentenceElement);
            }
            // Alternatives
            var radioGroup = $("<div/>").addClass("funkyradio");
            body.append(radioGroup);
            for (var i = 0; i < data.alternatives.length; ++i) {
                // Container
                var div = $("<div/>")
                    .addClass("funkyradio-primary")
                    .addClass("alternative");
                radioGroup.append(div);
                // Radio
                var radio = $("<input/>");
                div.append(radio);
                radio.attr("type", "radio");
                radio.attr("name", "single-choice-exercise");
                radio.attr("id", "single-choice-exercise" + (i + 1));
                radio.attr("value", data.alternatives[i]);
                radio.click(function() {
                    btnCheck.prop("disabled", false);
                });
                // Label
                var label = $("<label/>");
                label.attr("for", radio.attr("id"));
                div.append(label);
                label.append($("<span/>").text(i + 1 + ". "));
                label.append($("<span/>").text(data.alternatives[i]));
                // Keyboard mapping
                keyMap.number(
                    i + 1,
                    (function(radio) {
                        return function() {
                            radio.click();
                        };
                    })(radio)
                );
            }
            // Logic
            var send = function() {
                var chosenAlternative = radioGroup
                    .find("input:checked")
                    .attr("value");
                if (chosenAlternative === undefined) return;
                keyMap.clearNumber();
                submit({correctAlternative: chosenAlternative}).done(function(
                    response
                ) {
                    if (!response.success) {
                        console.log("Failure to submit answer");
                        return;
                    }
                    displayFeedbackFooter({correct: response.data.correct});
                    radioGroup.children().each(function() {
                        var value = $(this)
                            .find("input")
                            .attr("value");
                        if (value === response.data.correctAlternative)
                            $(this).attr(
                                "class",
                                "alternative funkyradio-correct"
                            );
                        else
                            $(this).attr(
                                "class",
                                "alternative funkyradio-wrong"
                            );
                    });
                    radioGroup.find("input").attr("disabled", "disabled");
                });
            };
            btnCheck.prop("disabled", true);
            btnCheck.click(send);
            keyMap.enter(send);
        } else if (data.type === "matching") {
            // Logic
            var len = data.left_items.length;
            var sel = null;
            var selectLeftItem = function(idx) {
                return function() {
                    // Change highlighted item
                    if (sel != null) left_items[sel].removeClass("active");
                    sel = idx;
                    left_items[sel].addClass("active");
                    // Enable/disable check button
                    for (var i = 0; i < len; ++i)
                        if (left_items[i].children(".number").text() === "?")
                            btnCheck.prop("disabled", true);
                    // Update keymap
                    keyMap.previous(selectPreviousLeftItem);
                    keyMap.next(selectNextLeftItem);
                };
            };
            var selectNextLeftItem = function() {
                selectLeftItem((sel + 1) % len)();
            };
            var selectPreviousLeftItem = function() {
                selectLeftItem((sel + len - 1) % len)();
            };
            var selectRightItem = function(idx) {
                return function() {
                    for (var i = 0; i < len; ++i)
                        if (left_items[i].children(".number").text() == idx + 1)
                            left_items[i].children(".number").text("?");
                    btnCheck.prop("disabled", false);
                    if (left_items[sel].children(".number").text() !== "?")
                        right_items[
                            parseInt(
                                left_items[sel].children(".number").text(),
                                10
                            ) - 1
                        ].removeClass("selected");
                    left_items[sel].children(".number").text(idx + 1);
                    right_items[idx].addClass("selected");
                    selectNextLeftItem();
                };
            };

            var send = function() {
                var orderedAlternatives = new Array(len);
                for (var i = 0; i < len; ++i) {
                    var text = left_items[i].children(".number").text();
                    if (text === "?") {
                        orderedAlternatives = null;
                        break;
                    }
                    var index = parseInt(text, 10) - 1;
                    orderedAlternatives[i] = right_items[index]
                        .find("span")
                        .text();
                }
                if (orderedAlternatives !== null) {
                    submit({orderedAlternatives: orderedAlternatives}).done(
                        function(response) {
                            displayFeedbackFooter({
                                correct: response.data.correct,
                            });
                            //TODO: display the correct order
                        }
                    );
                }
            };
            // Events
            for (var i = 0; i < len; ++i)
                keyMap.number(i + 1, selectRightItem(i));
            btnCheck.click(send);
            keyMap.enter(send);
            // Title
            var title = $("<div/>")
                .addClass("title")
                .html(data.title);
            body.append(title);
            // Table
            var table = $("<div/>").addClass("table");
            body.append(table);
            // Sentences
            for (let i = 0; i < data.sentences.length; ++i) {
                let sentence = data.sentences[i];
                let sentenceElement = $("<div/>")
                    .addClass("sentence")
                    .html(sentence.text);
                if (sentence.lojbanic) {
                    sentenceElement.addClass("lojbanic");
                }
                if (data.sentences.length === 1) {
                    sentenceElement.addClass("single");
                }
                table.append(sentenceElement);
            }
            // Linebreak
            body.append("<br/><br/>");
            // Left items
            var left = $("<div/>")
                .addClass("left")
                .addClass("list-group")
                .addClass("list-group-horizontal");
            body.append(left);
            var left_items = [];
            for (var i = 0; i < data.left_items.length; ++i) {
                var item = $("<a/>")
                    .addClass("list-group-item")
                    .click(selectLeftItem(i));
                left.append(item);
                left_items.push(item);
                var number = $("<span>")
                    .addClass("number")
                    .text("?");
                item.append(number);
                item.append(" &mdash; ");
                item.append($("<span/>").text(data.left_items[i]));
            }
            // Right items
            var right = $("<div/>")
                .addClass("right")
                .addClass("list-group");
            body.append(right);
            var right_items = [];
            for (var i = 0; i < data.right_items.length; ++i) {
                var item = $("<li/>")
                    .addClass("list-group-item")
                    .click(selectRightItem(i));
                right.append(item);
                right_items.push(item);
                item.append(i + 1 + ". ");
                var text = $("<span>").text(data.right_items[i]);
                item.append(text);
            }
            // Initialize
            selectLeftItem(0)();
        } else if (data.type === "typing") {
            // Title
            var title = $("<div/>")
                .addClass("title")
                .html(data.title);
            body.append(title);
            // Table
            var table = $("<div/>").addClass("table");
            body.append(table);
            // Sentences
            for (let i = 0; i < data.sentences.length; ++i) {
                let sentence = data.sentences[i];
                let sentenceElement = $("<div/>")
                    .addClass("sentence")
                    .html(sentence.text);
                if (sentence.lojbanic) {
                    sentenceElement.addClass("lojbanic");
                }
                table.append(sentenceElement);
            }
            if (data.sentences.length >= 2) {
                table = $("<div/>").addClass("table");
                body.append(table);
            }
            // Answer
            var textarea = $("<textarea/>");
            table.append(textarea);
            textarea.focus();
            // Submit
            var send = function() {
                submit({text: textarea.val()}).done(function(response) {
                    if (!response.success) {
                        console.log("Failure to submit answer");
                        return;
                    }
                    if (response.data.correct) {
                        if (textarea.val() === response.data.canonicalAnswer)
                            displayFeedbackFooter({correct: true});
                        else
                            displayFeedbackFooter({
                                correct: true,
                                alternativeAnswer:
                                    response.data.canonicalAnswer,
                            });
                    } else {
                        displayFeedbackFooter({
                            correct: false,
                            correctAnswer: response.data.canonicalAnswer,
                        });
                    }
                });
            };
            keyMap.enter(send);
            btnCheck.click(send);
            textarea.keydown(function(e) {
                if (e.keyCode == 13) {
                    send();
                    return false;
                }
            });
        }
        // Dictionary
        body.find(".sentence.lojbanic").each(function() {
            let $this = $(this);
            $this.html($this.text().replace(/\b([\w']+)\b/g, "<span class='lojbanic'>$1</span>"));
        });
        $("span.lojbanic").mouseover(function() {
            // Delete old tooltips
            $(".dictionary-tooltip-holder").remove();
            // Lookup definition
            let dictionary = window.dictionary || null;
            if (!dictionary || !dictionary.valsiDefinition) {
                return;
            }
            let word = $(this).text();
            let definitionText = dictionary.valsiDefinition[word];
            if (!definitionText) {
                return;
            }
            //definitionText = definitionText.replace(/x1/g, "x₁");
            //definitionText = definitionText.replace(/x2/g, "x₂");
            //definitionText = definitionText.replace(/x3/g, "x₃");
            //definitionText = definitionText.replace(/x4/g, "x₄");
            //definitionText = definitionText.replace(/x5/g, "x₅");
            // Calculate position
            let position = $(this).offset();
            position.left -= $(document).scrollLeft();
            position.top -= $(document).scrollTop();
            position.left += $(this).width() / 2;
            // Create tooltip
            let tooltipHolder = $("<div>").addClass("dictionary-tooltip-holder").css(position);
            let tooltip = $(tooltipHolder).addClass("dictionary-tooltip").css(position);
            let definitions = definitionText.split("; ");
            for (let i = 0; i < definitions.length; ++i) {
                let definition = definitions[i];
                let definitionElement = $("<div/>").addClass("dictionary-tooltip-definition").text(definition);
                tooltip.append(definitionElement);
            }
            tooltipHolder.append(tooltip);
            body.append(tooltip);
            // Animate tooltip
            tooltipHolder.css("opacity", 0);
            tooltipHolder.animate({opacity: 1}, 200);
        });
        $("span.lojbanic").mouseout(function() {
            // Delete tooltips
            $(".dictionary-tooltip-holder").remove();
        });
    };

    nextExercise();
    return {};
};

$(document).ready(function() {
    var holder = $("#exercise-holder");
    var manager = createExercisesManager(holder);
});