
// Create a webpage object
var page = require('webpage').create();

// Include the File System module for writing to files
var fs = require('fs');

// Capture arguments
var args = require('system').args;
var url = args[1]


// Specify source and path to output file
var path = 'sc.html'

// create waitfor function so page fully loads before saving
function waitFor(testFx, onReady, timeOutMillis) {
    var maxtimeOutMillis = timeOutMillis ? timeOutMillis : 10000, //< Default Max Timout is 3s
        start = new Date().getTime(),
        condition = false,
        interval = setInterval(function() {
            if ( (new Date().getTime() - start < maxtimeOutMillis) && !condition ) {
                // If not time-out yet and condition not yet fulfilled
                condition = (typeof(testFx) === "string" ? eval(testFx) : testFx()); //< defensive code
            } else {
                if(!condition) {
                    // If condition still not fulfilled (timeout but condition is 'false')
                    console.log("'waitFor()' timeout");
                    phantom.exit(1);
                } else {
                    // Condition fulfilled (timeout and/or condition is 'true')
                    console.log("'waitFor()' finished in " + (new Date().getTime() - start) + "ms.");
                    typeof(onReady) === "string" ? eval(onReady) : onReady(); //< Do what it's supposed to do once the condition is fulfilled
                    clearInterval(interval); //< Stop this interval
                }
            }
        }, 250); //< repeat check every 250ms
};

page.settings.userAgent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:91.0) Gecko/20100101 Firefox/91.0'
page.open(url, function (status) {
	if (status !== "success") {
		console.log("unable to load page");
		phantom.exit();
	} else {
	  console.log("page opened");
		// wait for price to be visible
		waitFor(function() {
            // Check in the page if a specific element is now visible
            return page.evaluate(function() {
                return $(".product-detail__price-text").is(":visible");
            });
        }, function() {
			console.log("Price should be visible now")
			page.render('sc.png');
			fs.write(path, page.content, 'w');
			phantom.exit();
    });
	}
});
