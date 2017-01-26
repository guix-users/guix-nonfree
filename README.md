### Guix Nonfree

An unofficial collection of packages that do not comply with the FSDG and are
therefore not going to be accepted in to guix.

## Usage

Clone this repo in to your home directory and add it to your `GUIX_PACKAGE_PATH`.

```sh
git clone https://github.com/guix-nonfree/guix-nonfree

export GUIX_PACKAGE_PATH='~/guix-nonfree'
```

## How to run GuixSD on any hardware

You'll need a custom guixsd installer so that all devices work correctly.

1. Replace the default gnu/system/examples/desktop.tmpl with the example
   configuration.

   WARNING: If you don't do this step the installer will work, but you'll end up
   with a system that can't connect to a network, so you'll have to restart the
   installation process.

   NOTE: By keeping the same version of linux and linux-firmware in the example
   and the installer you can avoid rebuilding the kernel during installation
   since it will be available in the installation image as a substitute.

2. Copy the linux-firmware and linux packages inside the gnu/system/install.scm
   and add all the relevant imports.
3. Configure installation-os by adding the snippet. (in gnu/system/install.scm)

```scheme
(kernel linux)
(firmware (list linux-firmware))
```

4. Build installation image with guix.

```sh
guix system disk-image gnu/system/install.scm --image-size=1.1G
```

5. Follow the usual installation steps from the guix manual.


# Example configuration

```scheme
(use-modules (guix packages)
             (guix download)
             (guix gexp)
             (guix git-download)
             (guix build-system trivial)
             (gnu)
             (gnu system nss))
(use-service-modules avahi base dbus desktop networking sddm xorg)
(use-package-modules admin certs gnome linux shells)

(define-public linux-firmware
  (let ((commit "6d3bc8886517d171068fd1263176b8b5c51df204"))
    (package
      (name "linux-firmware")
      (version (string-append "2017.01.26-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                        (url (string-append
                              "https://github.com/wkennington/linux-firmware"))
                        (commit commit)))
                (sha256
                 (base32
                  "15qm9fzv8rjhzyrqjdd4dqd6slymiz0w6wn7likbfjvh2szczafs"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))

           (let* ((source   (assoc-ref %build-inputs "source"))
                  (out      (assoc-ref %outputs "out"))
                  (firmware (string-append out "/lib/firmware")))
             (mkdir-p firmware)
             (copy-recursively source firmware)))))
      (home-page "https://github.com/wkennington/linux-firmware")
      (synopsis "Linux firmware")
      (description "Linux firmware.")
      (license #f))))

(define-public linux
  (package
    (inherit linux-libre)
    (version "4.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://cdn.kernel.org/pub/linux/kernel/v4.x/"
                    "linux-" version ".tar.xz"))
              (sha256
               (base32
                "16yfrydxcdlbm8vmfqirc0gshsbka6mjgfwc2wqs422v19vsz4zl"))))))

(operating-system
  (host-name "system76")
  (timezone "Europe/Zurich")
  (locale "en_US.UTF-8")

  (bootloader (grub-configuration (device "/dev/sda")))
  (kernel linux)
  (firmware (list linux-firmware))

  (file-systems (cons (file-system
                        (device "root")
                        (title 'label)
                        (mount-point "/")
                        (type "btrfs"))
                      %base-file-systems))

  (users (cons (user-account
                (name "dvc")
                (group "users")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video"))
                (home-directory "/home/dvc")
                (shell #~(string-append #$zsh "/bin/zsh")))
               %base-user-accounts))

  (packages (cons* nss-certs         ;for HTTPS access
                   %base-packages))

  (services (cons* (service network-manager-service-type
                    (network-manager-configuration))
                   (service wpa-supplicant-service-type wpa-supplicant)

                   (avahi-service)
                   (colord-service)
                   (dbus-service)
                   (elogind-service)
                   (geoclue-service)
                   (ntp-service)
                   (polkit-service)
                   (udisks-service)
                   (upower-service)

                   (sddm-service)
                   (gnome-desktop-service)
                   %base-services))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))
```
