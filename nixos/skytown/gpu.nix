# Based on https://nixos.wiki/wiki/AMD_GPU and
# https://nixos.org/manual/nixos/unstable/index.html#sec-gpu-accel-vulkan

{ config, pkgs, ... }:

{
  boot.initrd.kernelModules = [ "amdgpu" ];
  services.xserver.videoDrivers = [ "amdgpu" ];

  # HIP
  systemd.tmpfiles.rules = [
    "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.hip}"
  ];

  hardware.opengl.extraPackages = with pkgs; [
    rocm-opencl-icd
    rocm-opencl-runtime
    amdvlk
  ];

  environment.systemPackages = [
    pkgs.clinfo # to verify that OpenCL is configured correctly
  ];

  # vulkan (https://nixos.org/manual/nixos/unstable/index.html#sec-gpu-accel-vulkan)
  hardware.opengl.driSupport = true;

  # For 32 bit applications
  hardware.opengl.driSupport32Bit = true;

  # To enable Vulkan support for 32-bit applications, also add:
  hardware.opengl.extraPackages32 = [
    pkgs.driversi686Linux.amdvlk
  ];
}
