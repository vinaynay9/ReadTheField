import * as THREE from 'https://cdn.jsdelivr.net/npm/three@0.160.0/build/three.module.js';

(function () {
  const isFlagEnabled = (window.RTF_CURSOR_3D === true) || (String(window.RTF_CURSOR_3D) === '1');
  if (!isFlagEnabled) return;

  const prefersReduced = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  const isTouch = window.matchMedia && window.matchMedia('(pointer: coarse)').matches;
  if (prefersReduced || isTouch) return;

  const scene = new THREE.Scene();
  const camera = new THREE.PerspectiveCamera(50, window.innerWidth / window.innerHeight, 0.1, 100);
  camera.position.z = 6;

  const renderer = new THREE.WebGLRenderer({ alpha: true, antialias: true });
  renderer.setPixelRatio(window.devicePixelRatio || 1);
  renderer.setSize(window.innerWidth, window.innerHeight);
  renderer.domElement.style.position = 'fixed';
  renderer.domElement.style.top = '0';
  renderer.domElement.style.left = '0';
  renderer.domElement.style.width = '100%';
  renderer.domElement.style.height = '100%';
  renderer.domElement.style.pointerEvents = 'none';
  renderer.domElement.style.zIndex = '9';
  document.body.appendChild(renderer.domElement);

  const footballGeo = new THREE.SphereGeometry(0.5, 32, 32);
  footballGeo.scale(1.6, 1, 0.9);
  const footballMat = new THREE.MeshStandardMaterial({ color: 0x8b4a2b, roughness: 0.6, metalness: 0.1 });
  const football = new THREE.Mesh(footballGeo, footballMat);
  football.position.set(0, 0, 0);
  scene.add(football);

  const seamGeo = new THREE.TorusGeometry(0.18, 0.02, 8, 24);
  const seamMat = new THREE.MeshStandardMaterial({ color: 0xf5f5f5, roughness: 0.3, metalness: 0.2 });
  const seam = new THREE.Mesh(seamGeo, seamMat);
  seam.rotation.x = Math.PI / 2;
  football.add(seam);

  const ambient = new THREE.AmbientLight(0xffffff, 0.7);
  scene.add(ambient);
  const light = new THREE.DirectionalLight(0xffffff, 0.8);
  light.position.set(3, 4, 5);
  scene.add(light);

  const target = new THREE.Vector3(0, 0, 0);
  const current = new THREE.Vector3(0, 0, 0);

  function onPointerMove(event) {
    const x = (event.clientX / window.innerWidth) * 2 - 1;
    const y = -(event.clientY / window.innerHeight) * 2 + 1;
    target.set(x * 2.5, y * 1.6, 0);
  }

  function onResize() {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
  }

  window.addEventListener('mousemove', onPointerMove, { passive: true });
  window.addEventListener('resize', onResize, { passive: true });

  function animate() {
    current.lerp(target, 0.08);
    football.position.copy(current);
    football.rotation.y += 0.02;
    football.rotation.x = current.y * 0.2;
    football.rotation.z = current.x * -0.2;
    renderer.render(scene, camera);
    requestAnimationFrame(animate);
  }

  animate();
})();
