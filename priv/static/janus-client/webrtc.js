var localVideo;
var localStream;
var remoteVideo;
var peerConnection;
var janusConnection;
var sessionId;
var handleId;
var publisherId;
var roomInput
var feedInput

const CONFIG = {
  audio: true,
  video: true,
  iceServers: [
    {urls: 'stun:stun.l.google.com:19302'},
  ]
}

const RECEIVERS = {
  create_session,
  create_handle,
  create_room,
  join_publisher,
  publish,
  join_subscriber,
  ack
}

function page_ready() {
  localVideo = document.getElementById('localVideo');
  remoteVideo = document.getElementById('remoteVideo');
  roomInput = document.getElementById('room_id');
  feedInput = document.getElementById('feed_id');

  janusConnection = new WebSocket('ws://' + window.location.hostname + ':8188', 'janus-protocol');
  janusConnection.onmessage = function(message) {
    payload = JSON.parse(message.data);
    receiver = RECEIVERS[payload.janus] || RECEIVERS[payload.transaction] || console.log;
    receiver(payload);
  }
}

function start_session() {
  msg = JSON.stringify({janus: 'create', transaction: 'create_session'});
  janusConnection.send(msg);
}

function start_handle() {
  msg = JSON.stringify({janus: 'attach', transaction: 'create_handle', plugin: 'janus.plugin.videoroom', session_id: sessionId});
  janusConnection.send(msg);
}

function make_room() {
  msg = JSON.stringify({janus: 'message', transaction: 'create_room', body: {request: 'create'}, session_id: sessionId, handle_id: handleId});
  janusConnection.send(msg);
}

function join_room_publisher() {
  roomId = parseInt(roomInput.value);
  msg = JSON.stringify({janus: 'message', transaction: 'join_publisher', body: {request : 'join', ptype: 'publisher', room: roomId}, session_id: sessionId, handle_id: handleId});
  janusConnection.send(msg);
}

function start_broadcast() {
  peerConnection = new RTCPeerConnection(CONFIG);
  peerConnection.onicecandidate = on_ice_candidate;
  peerConnection.ontrack = on_track;

  navigator.mediaDevices.getUserMedia(CONFIG).then(function(stream) {
    localVideo.srcObject = stream;
    peerConnection.addStream(stream);

    peerConnection.createOffer().then(function(offer) {
      peerConnection.setLocalDescription(offer);
      msg = JSON.stringify({janus: 'message', transaction: 'publish', body: {request: 'publish'}, jsep: offer, session_id: sessionId, handle_id: handleId});
      janusConnection.send(msg);
    });
  });
}

function join_broadcast() {
  roomId = parseInt(roomInput.value);
  feedId = parseInt(feedInput.value);

  peerConnection = new RTCPeerConnection(CONFIG);
  peerConnection.onicecandidate = on_ice_candidate;
  peerConnection.ontrack = on_track;

  msg = JSON.stringify({janus: 'message', transaction: 'join_subscriber', body: {request : 'join', ptype: 'subscriber', room: roomId, feed: feedId}, session_id: sessionId, handle_id: handleId});
  janusConnection.send(msg);
}

// RCTPeerConnection on
function on_ice_candidate(event) {
  msg = JSON.stringify({janus: 'trickle', transaction: 'candidate', candidate: event.candidate, session_id: sessionId, handle_id: handleId});
  janusConnection.send(msg);
}

function on_track(event) {
  remoteVideo.srcObject = event.streams[0];
}

// Receivers
function create_session(payload) {
  sessionId = payload.data.id;
  setInterval(keepalive, 30);
}

function create_handle(payload) {
  handleId = payload.data.id;
}

function create_room(payload) {
  roomId = payload.plugindata.data.room;
  roomInput.setAttribute('value', roomId);
}

function join_publisher(payload) {
  feedInput.setAttribute('value', payload.plugindata.data.id);
}

function publish(payload) {
  answer = payload.jsep;
  peerConnection.setRemoteDescription(new RTCSessionDescription(answer));
  // peerConnection.addIceCandidate(new RTCIceCandidate(answer.sdp));
}

function join_subscriber(payload) {
  peerConnection.setRemoteDescription(new RTCSessionDescription(payload.jsep));
  peerConnection.createAnswer().then(function(answer) {
    peerConnection.setLocalDescription(answer);
    msg = JSON.stringify({janus: 'message', transaction: 'blah', body: {request: 'start'}, jsep: answer, session_id: sessionId, handle_id: handleId});
    janusConnection.send(msg);
  });
}

function keepalive() {
    msg = JSON.stringify({janus: 'keepalive', transaction: 'keepalive', session_id: sessionId});
    janusConnection.send(msg);
}

function ack(payload) {}
