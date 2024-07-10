# Delete file if it exists
if(length(list.files(pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", recursive = T))>0) {
    file.remove(list.files(pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", recursive = T))
}

# if(length(list.files(path = "data", pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)"))>0) {
#     for(i in 1:length(list.files(path = "data", pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)"))) {
#         file.remove(list.files(path = "data", pattern = "asreml+(([a-zA-Z0-9_.\\-])*)+(.zip|.tar.gz|.tgz)", )[i])
#     }
# }

if(dir.exists("data")) {
    unlink("data", recursive = T)
}
