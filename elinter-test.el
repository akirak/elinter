;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'elinter)

(describe "elinter--main-file-p"
  (it "Checks if the file is a main file"
    (expect (elinter--main-file-p "elinter.el") :to-be-truthy)
    (expect (elinter--main-file-p "elinter-test.el") :to-be nil)))

(describe "Conversion between fetcher specs and remote URLs"

  (describe "elinter--build-fetcher-spec"
    (it "github and gitlab fetcher"
      (expect (elinter--build-fetcher-spec 'github "akirak/elinter")
              :to-equal '(:fetcher github :repo "akirak/elinter"))
      (expect (elinter--build-fetcher-spec 'gitlab "akirak/elinter")
              :to-equal '(:fetcher gitlab :repo "akirak/elinter")))
    (it "git fetcher"
      (expect (elinter--build-fetcher-spec 'git "git@xxx.com:12345.git")
              :to-equal '(:fetcher git :url "git@xxx.com:12345.git"))))

  (describe "elinter--default-repo-name"
    (it "produces a repository name from a directory"
      (expect (elinter--default-repo-name "/home/user/parent/sample-project/")
              :to-equal "sample-project")))

  (describe "elinter--origin-url-from-config-lines"
    (it "extracts a remote URL from configuration lines"
      (expect (elinter--origin-url-from-config-lines
               (split-string "core.bare=false
core.logallrefupdates=true
remote.origin.url=https://github.com/akirak/elinter.git
remote.origin.fetch=+refs/heads/*:refs/remotes/origin/*" "\n"))
              :to-equal "https://github.com/akirak/elinter.git"))
    (it "returns nil if none is found"
      (expect (elinter--origin-url-from-config-lines nil)
              :to-be nil)))

  (describe "elinter--url-to-fetcher-spec"
    (it "github"
      (expect (elinter--url-to-fetcher-spec "git@github.com:user/repo.git")
              :to-equal '(:fetcher github :repo "user/repo"))
      (expect (elinter--url-to-fetcher-spec "https://github.com/user/repo.git")
              :to-equal '(:fetcher github :repo "user/repo"))
      (expect (elinter--url-to-fetcher-spec "https://github.com/user/repo")
              :to-equal '(:fetcher github :repo "user/repo"))
      (expect (elinter--url-to-fetcher-spec "https://github.com/user/repo/")
              :to-equal '(:fetcher github :repo "user/repo")))

    (it "gitlab"
      (expect (elinter--url-to-fetcher-spec "git@gitlab.com:user/repo.git")
              :to-equal '(:fetcher gitlab :repo "user/repo"))
      (expect (elinter--url-to-fetcher-spec "https://gitlab.com/user/repo.git")
              :to-equal '(:fetcher gitlab :repo "user/repo"))
      (expect (elinter--url-to-fetcher-spec "https://gitlab.com/user/repo")
              :to-equal '(:fetcher gitlab :repo "user/repo"))
      (expect (elinter--url-to-fetcher-spec "https://gitlab.com/user/repo/")
              :to-equal '(:fetcher gitlab :repo "user/repo")))

    (it "git"
      (expect (elinter--url-to-fetcher-spec "git://user@host.com/repo.git")
              :to-equal '(:fetcher git :url "git://user@host.com/repo.git"))))

  (describe "elinter--spec-to-url"
    (describe "elinter-use-https-url is t"
      (it "generates an HTTPS url"
        (expect (let ((elinter-use-https-url t))
                  (elinter--spec-to-url '(:fetcher github :repo "user/repo")))
                :to-equal "https://github.com/user/repo.git")
        (expect (let ((elinter-use-https-url t))
                  (elinter--spec-to-url '(:fetcher gitlab :repo "user/repo")))
                :to-equal "https://gitlab.com/user/repo.git")))
    (describe "elinter-use-https-url is nil"
      (it "generates an SSH url"
        (expect (let ((elinter-use-https-url nil))
                  (elinter--spec-to-url '(:fetcher github :repo "user/repo")))
                :to-equal "git@github.com:user/repo.git")
        (expect (let ((elinter-use-https-url nil))
                  (elinter--spec-to-url '(:fetcher gitlab :repo "user/repo")))
                :to-equal "git@gitlab.com:user/repo.git")))
    (describe "The fetcher is git"
      (it "generates an Git url"
        (expect (elinter--spec-to-url '(:fetcher git :url "git://host.com/repo.git"))
                :to-equal "git://host.com/repo.git")))))

(provide 'elinter-test)
