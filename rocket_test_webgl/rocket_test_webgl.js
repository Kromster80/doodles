"use strict";

if (!Detector.webgl) Detector.addGetWebGLMessage();

var camera, controls, scene, renderer;

// Globals for simplicity
var
    gRocket = {obj: undefined},
    gTarget = {obj: undefined};

init();
animate();

function init() {

    scene = new THREE.Scene();

    renderer = new THREE.WebGLRenderer({antialias: true});
    renderer.setPixelRatio(window.devicePixelRatio);
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.body.appendChild(renderer.domElement);

    // camera
    camera = new THREE.PerspectiveCamera(40, window.innerWidth / window.innerHeight, 1, 1000);
    camera.position.set(15, 20, 30);
    scene.add(camera);

    // controls
    controls = new THREE.OrbitControls(camera, renderer.domElement);
    controls.minDistance = 20;
    controls.maxDistance = 50;
    controls.maxPolarAngle = Math.PI / 2;

    scene.add(new THREE.AmbientLight(0x222222));

    var light = new THREE.PointLight(0xffffff, 1);
    camera.add(light);

    scene.add(new THREE.AxisHelper(10));

    initSceneObjects();

    window.addEventListener('resize', onWindowResize, false);

}

function initSceneObjects() {

    // Construct "Rocket"
    gRocket.obj = new THREE.Group();
    scene.add(gRocket.obj);

    var mtlRocket = new THREE.MeshLambertMaterial({color: 0xffffff});
    var geoRocket = new THREE.CylinderGeometry(2, 2, 10, 24);
    var meshRocket = new THREE.Mesh(geoRocket, mtlRocket);
    gRocket.obj.add(meshRocket);

    // Construct "Target"
    gTarget.obj = new THREE.Group();
    scene.add(gTarget.obj);

    var mtlTarget = new THREE.MeshLambertMaterial({color: 0x4080ff});
    var geoTarget = new THREE.CubeGeometry(1, 1, 1);
    var meshTarget = new THREE.Mesh(geoTarget, mtlTarget);
    gTarget.obj.add(meshTarget);
}

function onWindowResize() {

    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();

    renderer.setSize(window.innerWidth, window.innerHeight);

}

function animate() {

    requestAnimationFrame(animate);

    gRocket.obj.rotation.y += 0.005;

    render();

}

function render() {

    renderer.render(scene, camera);

}

