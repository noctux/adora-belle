// Initialised with defaults
var preferences = {
	usesound: false,
	storelogindata: false,
	markrecentlydeferred: false,
};
function loadPreferences() {
	var stored;
	var supported = false;
	if (typeof(Storage) !== "undefined") {
		try {
			stored = localStorage.getItem("preferences");
			supported = true;
		} catch (e) {}
	}
	if (supported) {
		if (stored) {
			try {
				var parsed = JSON.parse(stored);
				if (parsed) {
					preferences = parsed;
					console.log("parsed preferences");
					console.log(preferences);
				} else {
					console.log("Found invalid value in parsed JSON state");
				}
			} catch(e) {
				console.log("Error parsing preferences from localstorage, skipping. Input was: " + stored);
			}
		} else {
			console.log("No stored preferences found, keeping defaults");
		}
	} else {
		console.log("Browser does not support local storage, no persistent storage available");
	}
}
loadPreferences();

function storePreferences() {
	if (typeof(Storage) !== "undefined") {
		localStorage.setItem("preferences", JSON.stringify(preferences));
		console.log("Stored preferences to local storage");
	} else {
		console.log("Browser does not support local storage, no persistent storage available");
	}
}

function getPreference(id) {
	return preferences[id];
}

function setPreference(id, newValue) {
	preferences[id] = newValue;
	storePreferences();
}

function setupPreferencesmodal() {
	var body = $('<div class="modal-body" />');
	var available = ['storelogindata'];
	if (admininterface) {
		available.push('markrecentlydeferred');
		available.push('usesound');
	}
	var config = {
		storelogindata: {
			help: "Store logincredentials on this device",
			handler: function(setting, newValue) {
				if (newValue) {
					setPreference('username', username);
					setPreference('password', password);
				} else {
					setPreference('username', undefined);
					setPreference('password', undefined);
				}
				setPreference(setting, newValue);
			},
		},
		markrecentlydeferred: {
			help: "Mark recently deferred requests in lists",
		},
		usesound: {
			help: "Notify me by sound on new activity",
		}
	};
	available.forEach(function(settingsname){
		var cfg = config[settingsname];
		var div = $('<div class="form-group form-check" />');
		var input = $('<input type="checkbox" class="form-check-input"/>');
		input.prop('id', settingsname);
		div.append(input);
		// Set the current state
		if (getPreference(settingsname)) {
			input.attr('checked', true);
		}
		var label = $('<label class="form-check-label"/>');
		label.prop('for', settingsname);
		label.text(cfg.help);
		div.append(label);
		input.change(function () {
			if (cfg['handler']) {
				cfg.handler(settingsname, input.is(':checked'));
			} else {
				setPreference(settingsname, input.is(':checked'));
			}
		});
		body.append(div);
	});
	$('#preferenceform').append(body);
}

function testBasicCredentials(endpoint, username, password) {
	var success = false;

	$.ajax({
		type: "GET",
		url: endpoint,
		beforeSend: function (xhr) {
			xhr.setRequestHeader ("Authorization", "Basic " + btoa(username + ":" + password));
		},
		dataType: 'json',
		async: false,
		success: function (){success = true;},
		error: function(){ success = false; alert("Authentication failed: wrong username or password"); }
	});
	return success;
}

function showError(msg) {
	var elem = $('<div class="alert alert-danger alert-dismissible fade show" role="alert"></div>').text(msg);
	elem.append('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>');
	$('#errPanel').append(elem);
}

function registerReloadButton() {
	var elem = $('<button id="configreloadBtn" class="btn btn-sm btn-outline-primary my-2 my-sm-0" type="submit">Reload Config</button>');
	$('#navbarbuttonarea').append(elem);
}

function registerAwayButton() {
	var elem = $('<button id="awayBtn" class="btn btn-sm btn-outline-primary my-2 my-sm-0" type="submit">Away</button>');
	$('#navbarbuttonarea').append(elem);
}

var username;
var password;
var informationendpoint = "public/requests";
var conferencebaseurl;
var predictableroomnames;
var roomnameprefix;
var admininterface = false;
if (window.location.pathname.split('/').pop() == "admin.html") {
	informationendpoint = "admin/requests";
	admininterface = true;
	registerReloadButton();
	registerAwayButton();
}

function triggerConfigReload() {
	$.ajax({
		type: "POST",
		contentType: "application/json; charset=utf-8",
		url: "admin/reload",
		dataType: 'json',
		data: ""
	}).done(function (data) {
		updateUi();
	}).fail(function (err) {
		showError(err.responseText);
	});
}

function triggerAwayMessage(msg, show) {
	$.ajax({
		type: "POST",
		contentType: "application/json; charset=utf-8",
		url: "admin/away",
		dataType: 'json',
		data: JSON.stringify([msg, show])
	}).done(function (data) {
		updateUi();
	}).fail(function (err) {
		showError(err.responseText);
	});
}

function requestHelp(type) {
	$.ajax({
		type: "POST",
		contentType: "application/json; charset=utf-8",
		url: "public/request",
		dataType: 'json',
		data: JSON.stringify(type)
	}).done(function (data) {
		updateUi();
	}).fail(function (err) {
		showError(err.responseText);
	});
}

function cancelRequest(id) {
	var payload = {
		tag: "RequestID",
		contents: id
	};
	$.ajax({
		type: "POST",
		contentType: "application/json; charset=utf-8",
		url: "public/cancel",
		dataType: 'json',
		data: JSON.stringify(payload)
	}).done(function (data) {
		updateUi();
	}).fail(function (err) {
		showError(err.responseText);
	});
}

function adminaction(id, verb) {
	var payload = [{
		tag: "RequestID",
		contents: id
	}, verb]
	$.ajax({
		type: "POST",
		contentType: "application/json; charset=utf-8",
		url: "admin/handle",
		dataType: 'json',
		data: JSON.stringify(payload)
	}).done(function (data) {
		updateUi();
	}).fail(function (err) {
		showError(err.responseText);
	});
}

function formatUrl(req) {
	var urlsuffix;
	if (predictableroomnames) {
		var user;
		if (admininterface) {
			user = req.userid.contents;
		} else {
			user = username;
		}
		urlsuffix = roomnameprefix + "-" + user;
	} else {
		urlsuffix = req.reqid.contents;
		// Some jitsiinstances do not like none alphanumeric characters
		urlsuffix = urlsuffix.replace(/[^0-9a-zA-Z]/gi, '');
	}

	return conferencebaseurl + "/" + encodeURIComponent(urlsuffix);
}

function formatTime(time) {
	var ts   = new Date(time);
	var now  = new Date();
	var diff = (now.getTime() - ts.getTime()) / 1000; // Seconds
	if (diff > 60) {
		return (diff / 60).toFixed(0) + "min";
	} else {
		return diff.toFixed(0) + "s";
	}
}

function formatRequest(req, context=null) {
	var elem;

	if (context == "dummy") {
		elem = $('<div class="list-group-item list-group-item-light"></div>');
		elem.text("Inactive");
	} else if (req.reqid.tag == "BlindedID" || req.userid.tag == "Anonymous") {
		elem = $('<div class="list-group-item list-group-item-light"></div>');
		elem.text(req.reqtype);
	} else {
		var url = formatUrl(req);
		if (context == "log") {
			elem = $('<div class="list-group-item list-group-item-secondary align-items-center text-left d-flex flex-wrap justify-content-between"></div>');
		} else if (req.reqtype == "Submission") {
			elem = $('<div class="list-group-item list-group-item-success align-items-center text-left d-flex flex-wrap justify-content-between"></div>');
			elem.prop('id', req.reqid.contents);
		} else {
			elem = $('<div class="list-group-item list-group-item-primary align-items-center text-left d-flex flex-wrap justify-content-between"></div>');
			elem.prop('id', req.reqid.contents);
		}
		var typedate = $('<div style="text-align: center"></div>');
		typedate.text(req.reqtype);
		if (admininterface) {
			if (context == "log") {
				typedate.append(" ");
			} else {
				typedate.append('<br/>');
			}
			typedate.append(formatTime(req.time) + " ago");
			elem.append(typedate);
		}
		elem.append(typedate);
		if (admininterface) {
			var userid = $('<div></div>');
			userid.text(req.userid.contents);
			elem.append(userid);
		}
		var a = $('<a target="_blank" rel="noopener noreferrer" />');
		a.attr("href", url);
		a.text(url);
		elem.append(a);
		var actions;
		var template = $('<button type="button" class="btn btn-secondary btn-sm" />');
		if (admininterface) {
			actions = $('<div class="btn-group" role="group" aria-label="actions" />');
			if (context == "active") {
				var defer    = template.clone().text("Defer").on('click', function () { adminaction(req.reqid.contents, "Defer") });
				var complete = template.clone().text("Complete").on('click', function () { adminaction(req.reqid.contents, "Complete") });
				actions.append(defer);
				actions.append(complete);
			} else if (context == "pending") {
				var handle = template.clone().text("Handle").on('click', function () { adminaction(req.reqid.contents, "Handle") });
				var discard = template.clone().text("Discard").on('click', function () { adminaction(req.reqid.contents, "Discard") });
				actions.append(handle);
				actions.append(discard);
			}
		} else {
			actions = template.text("Cancel");
			actions.on('click', function () { cancelRequest(req.reqid.contents) })
		}
		elem.append(actions);
	}
	return elem;
}

function formatBacklogItem(item) {
	var log = $('<li class="list-group-item" style="text-align: left" />');
	log.append(item.actor + ": " + item.action.contents + " (" +  formatTime(item.timeStamp) + " ago)");
	var reqs = $('<ul class"list-group" />')
	item.requests.map(x => reqs.append(formatRequest(x, "log")));
	log.append(reqs);
	return log;
}

var seenAdmins = [];

function updateServicedRequests(servicedRequests) {
	var order;
	if (admininterface) {
		Object.keys(servicedRequests).forEach(function (elem) {
			if(!seenAdmins.includes(elem)) {
				seenAdmins.push(elem);
			}
		});
		order = seenAdmins;
	} else {
		order = Object.keys(servicedRequests).sort();
	}

	var elem = $('#servicedReqs').clone().empty();
	order.forEach(function (key) {
		var lbl = $('<label class="d-flex justify-content-start col-form-label-lg"/>');
		lbl.text(key);
		elem.append(lbl);
		if (key in servicedRequests) {
			elem.append(formatRequest(servicedRequests[key], "active"));
		} else {
			elem.append(formatRequest(undefined, "dummy"));
		}
	});
	$('#servicedReqs').replaceWith(elem);
}

const seen = new Set();
let firstUpdate = true;

function updatePendingRequests(pendingRequests) {
	var elem = $('#pendingReqs').clone().empty();
	var before = seen.size;
	pendingRequests.forEach(x => {
		if (x.reqid.contents && !seen.has(x.reqid.contents)) {
			seen.add(x.reqid.contents);
		}
		elem.append(formatRequest(x, "pending"));
	});
	if (admininterface && getPreference('usesound') && !firstUpdate && seen.size > before) {
		new Audio("static/sound.opus").play();
	}
	firstUpdate = false;
	$('#pendingReqs').replaceWith(elem);
}

function updateAdministrativeLog(logItems) {
	var elem = $('#backlogitems').clone().empty();
	logItems.map(x => elem.append(formatBacklogItem(x)));
	$('#backlogitems').replaceWith(elem);
}

function markRecentlyDeferred(logItems) {
	var now  = new Date();
	for (var i = 0; i < logItems.length; i++) {
		var ts   = new Date(logItems[i].timeStamp);
		var diff = (now.getTime() - ts.getTime()) / 1000; // Seconds
		if (diff > 60){
			break;
		}
		if (logItems[i].action.contents !== "Defer") {
			continue;
		}
		logItems[i].requests.forEach(function (req){
			$('#' + req.reqid.contents)
				.removeClass('list-group-item-primary list-group-item-success')
				.addClass('list-group-item-warning');
		});
	}
}

function isActive(day, start, end) {
	var now = new Date;
	if (day !== ["sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"][now.getDay()])
		return false;
	var h = now.getHours();
	var m = now.getMinutes();
	if (h < start[0] || (h == start[0] && m < start[1]))
		return false;
	if (h > end[0] || (h == end[0] && m > end[1]))
		return false;
	return true;
}

function formatTimeSlot(ts) {
	var elem = $('<li/>');
	var day = ts.day.charAt(0).toUpperCase() + ts.day.slice(1);
	var start = ts.start.split(':').slice(0,2)
	var end   = ts.end.split(':').slice(0,2)
	elem.text(day + ": " + start.join(':') + " - " + end.join(':'));
	if (isActive(ts.day, start, end)) {
		elem[0].style.fontWeight = 900;
	}
	return elem;
}

function updateTimeSlots(timeslots) {
	var elem;
	if (timeslots.length <= 0) {
		elem = $('<div id="timeSlots" class="list-group" >Timeslots appear to be unrestricted</div>');
	} else {
		elem = $('<ul id="timeSlots" class="list-group" style="list-style: none"></div>');
		timeslots.map(x => elem.append(formatTimeSlot(x)));
	}
	$('#timeSlots').replaceWith(elem);
}

function updateLectureName(lectureName) {
	$('#lectureName').text(lectureName);
}

function updateAwayMessage(awayMsg, show) {
	$('#awayMsg').text(awayMsg);
	if (show) {
		$('#awayMsg').show();
		if (admininterface) {
			$('#awayBtn').text("Back");
		}
	} else {
		$('#awayMsg').hide();
		if (admininterface) {
			$('#awayBtn').text("Away");
		}
	}
}


function updateUi() {
	$.get(informationendpoint, {
		dataType: 'json',
	}).done(function (data) {
		conferencebaseurl = data.conferenceUrl;
		predictableroomnames = data.predictableNames;
		roomnameprefix = data.roomPrefix;
		updateLectureName(data.lectureName);
		updateAwayMessage(data.awayMsg, data.showAwayMsg);
		updateTimeSlots(data.timeSlots);
		updatePendingRequests(data.pendingRequests);
		updateServicedRequests(data.activeRequests);
		if (admininterface) {
			if(getPreference('markrecentlydeferred')) {
				markRecentlyDeferred(data.actionLog);
			}
			updateAdministrativeLog(data.actionLog);
		}
	}).fail(function (err) {
		console.log("An error occurred");
		console.log(err);
		showError("Updating request state failed");
	});
}

function relayoutWidth(window) {
	var wclass;
	if (window.width() < 1200) {
		wclass = "w-100";
	} else if (window.width() < 1800) {
		wclass = "w-75";
	} else {
		wclass = "w-50";
	}

	['#errPanel', '#servicedReqs', '#pendingReqs'].forEach(function (elem) {
		var domitem = $(elem);
		['w-100', 'w-75', 'w-50'].forEach(c => domitem.removeClass(c));
		domitem.addClass(wclass);
	});
}

function initApi(user, pw) {
	username = user;
	password = pw;

	$.ajaxSetup({
		beforeSend: function (xhr) {
			xhr.setRequestHeader ("Authorization", "Basic " + btoa(username + ":" + password));
		},
		cache: false,
	});

	if (!admininterface) {
		$.ajax
		({
			type: "GET",
			url: "admin/requests",
			dataType: 'json',
			async: false,
			success: function () {
				alert("Admins should use the admin endpoint (/admin.html), redirecting...");
				window.location.href = "admin.html";
			},
			error: function(){}
		});
	}

	$(window).on('resize', function() {
		relayoutWidth($(this));
	});
	relayoutWidth($(window));

	updateUi();

	if (admininterface) {
		$('#questionBtn').detach();
		$('#submissionBtn').detach();
		$('#configreloadBtn').on("click", function (e) { triggerConfigReload() });
		$('#awayBtn').on("click", function (e) {
			if ($('#awayBtn').text() === "Away") {
				$('#awayMsgInput').val($('#awayMsg').text());
				$('#awayModal').modal({ show: false});
				$('#awayModal').modal('show');
			} else {
				triggerAwayMessage($('#awayMsg').text(), false)
			}
		});
		$('#backlogheader').css("display", "initial");
	} else {
		$('#questionBtn').on("click",     function (e) { requestHelp("Question")   });
		$('#submissionBtn').on("click",   function (e) { requestHelp("Submission") });
	}

	window.setInterval(updateUi, 2000);
}

$('#loginform').on('submit', function(event) {
	event.preventDefault();
	var user = $('#usernameInput').val();
	var pw   = $('#passwordInput').val();
	if (testBasicCredentials(informationendpoint, user, pw)) {
		initApi(user, pw);
		$('#loginModal').modal('hide');
	} else {
		$('#passwordInput').val('');
	}
});

$('#loginModal').on('shown.bs.modal', function () {
	$('#usernameInput').trigger('focus');
});

setupPreferencesmodal();

var storeduser = getPreference('username');
var storedpw   = getPreference('password');
if (storeduser && storedpw && testBasicCredentials(informationendpoint, storeduser, storedpw)) {
	initApi(storeduser, storedpw);
	if (admininterface && getPreference('usesound')) {
		$('#autoPlayModal').modal('show');
	}
} else {
	$('#loginModal').modal({ show: false});
	$('#loginModal').modal('show');
}

$('#awayform').on('submit', function(event) {
	event.preventDefault();
	var msg = $('#awayMsgInput').val();
	$('#awayModal').modal('hide');
	triggerAwayMessage(msg, true);
});
