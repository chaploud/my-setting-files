_wasmer() {
    local i cur prev opts cmd
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    cmd=""
    opts=""

    for i in ${COMP_WORDS[@]}
    do
        case "${cmd},${i}" in
            ",$1")
                cmd="wasmer"
                ;;
            wasmer,add)
                cmd="wasmer__add"
                ;;
            wasmer,app)
                cmd="wasmer__app"
                ;;
            wasmer,auth)
                cmd="wasmer__auth"
                ;;
            wasmer,binfmt)
                cmd="wasmer__binfmt"
                ;;
            wasmer,cache)
                cmd="wasmer__cache"
                ;;
            wasmer,compile)
                cmd="wasmer__compile"
                ;;
            wasmer,config)
                cmd="wasmer__config"
                ;;
            wasmer,container)
                cmd="wasmer__container"
                ;;
            wasmer,create-exe)
                cmd="wasmer__create__exe"
                ;;
            wasmer,create-obj)
                cmd="wasmer__create__obj"
                ;;
            wasmer,deploy)
                cmd="wasmer__deploy"
                ;;
            wasmer,domain)
                cmd="wasmer__domain"
                ;;
            wasmer,gen-c-header)
                cmd="wasmer__gen__c__header"
                ;;
            wasmer,gen-completions)
                cmd="wasmer__gen__completions"
                ;;
            wasmer,gen-man)
                cmd="wasmer__gen__man"
                ;;
            wasmer,help)
                cmd="wasmer__help"
                ;;
            wasmer,init)
                cmd="wasmer__init"
                ;;
            wasmer,inspect)
                cmd="wasmer__inspect"
                ;;
            wasmer,journal)
                cmd="wasmer__journal"
                ;;
            wasmer,login)
                cmd="wasmer__login"
                ;;
            wasmer,namespace)
                cmd="wasmer__namespace"
                ;;
            wasmer,package)
                cmd="wasmer__package"
                ;;
            wasmer,publish)
                cmd="wasmer__publish"
                ;;
            wasmer,run)
                cmd="wasmer__run"
                ;;
            wasmer,self-update)
                cmd="wasmer__self__update"
                ;;
            wasmer,ssh)
                cmd="wasmer__ssh"
                ;;
            wasmer,validate)
                cmd="wasmer__validate"
                ;;
            wasmer,wast)
                cmd="wasmer__wast"
                ;;
            wasmer,whoami)
                cmd="wasmer__whoami"
                ;;
            wasmer__app,create)
                cmd="wasmer__app__create"
                ;;
            wasmer__app,database)
                cmd="wasmer__app__database"
                ;;
            wasmer__app,delete)
                cmd="wasmer__app__delete"
                ;;
            wasmer__app,deploy)
                cmd="wasmer__app__deploy"
                ;;
            wasmer__app,deployment)
                cmd="wasmer__app__deployment"
                ;;
            wasmer__app,get)
                cmd="wasmer__app__get"
                ;;
            wasmer__app,help)
                cmd="wasmer__app__help"
                ;;
            wasmer__app,info)
                cmd="wasmer__app__info"
                ;;
            wasmer__app,list)
                cmd="wasmer__app__list"
                ;;
            wasmer__app,logs)
                cmd="wasmer__app__logs"
                ;;
            wasmer__app,purge-cache)
                cmd="wasmer__app__purge__cache"
                ;;
            wasmer__app,region)
                cmd="wasmer__app__region"
                ;;
            wasmer__app,secret)
                cmd="wasmer__app__secret"
                ;;
            wasmer__app,version)
                cmd="wasmer__app__version"
                ;;
            wasmer__app,volume)
                cmd="wasmer__app__volume"
                ;;
            wasmer__app__database,help)
                cmd="wasmer__app__database__help"
                ;;
            wasmer__app__database,list)
                cmd="wasmer__app__database__list"
                ;;
            wasmer__app__database__help,help)
                cmd="wasmer__app__database__help__help"
                ;;
            wasmer__app__database__help,list)
                cmd="wasmer__app__database__help__list"
                ;;
            wasmer__app__deployment,get)
                cmd="wasmer__app__deployment__get"
                ;;
            wasmer__app__deployment,help)
                cmd="wasmer__app__deployment__help"
                ;;
            wasmer__app__deployment,list)
                cmd="wasmer__app__deployment__list"
                ;;
            wasmer__app__deployment,logs)
                cmd="wasmer__app__deployment__logs"
                ;;
            wasmer__app__deployment__help,get)
                cmd="wasmer__app__deployment__help__get"
                ;;
            wasmer__app__deployment__help,help)
                cmd="wasmer__app__deployment__help__help"
                ;;
            wasmer__app__deployment__help,list)
                cmd="wasmer__app__deployment__help__list"
                ;;
            wasmer__app__deployment__help,logs)
                cmd="wasmer__app__deployment__help__logs"
                ;;
            wasmer__app__help,create)
                cmd="wasmer__app__help__create"
                ;;
            wasmer__app__help,database)
                cmd="wasmer__app__help__database"
                ;;
            wasmer__app__help,delete)
                cmd="wasmer__app__help__delete"
                ;;
            wasmer__app__help,deploy)
                cmd="wasmer__app__help__deploy"
                ;;
            wasmer__app__help,deployment)
                cmd="wasmer__app__help__deployment"
                ;;
            wasmer__app__help,get)
                cmd="wasmer__app__help__get"
                ;;
            wasmer__app__help,help)
                cmd="wasmer__app__help__help"
                ;;
            wasmer__app__help,info)
                cmd="wasmer__app__help__info"
                ;;
            wasmer__app__help,list)
                cmd="wasmer__app__help__list"
                ;;
            wasmer__app__help,logs)
                cmd="wasmer__app__help__logs"
                ;;
            wasmer__app__help,purge-cache)
                cmd="wasmer__app__help__purge__cache"
                ;;
            wasmer__app__help,region)
                cmd="wasmer__app__help__region"
                ;;
            wasmer__app__help,secret)
                cmd="wasmer__app__help__secret"
                ;;
            wasmer__app__help,version)
                cmd="wasmer__app__help__version"
                ;;
            wasmer__app__help,volume)
                cmd="wasmer__app__help__volume"
                ;;
            wasmer__app__help__database,list)
                cmd="wasmer__app__help__database__list"
                ;;
            wasmer__app__help__deployment,get)
                cmd="wasmer__app__help__deployment__get"
                ;;
            wasmer__app__help__deployment,list)
                cmd="wasmer__app__help__deployment__list"
                ;;
            wasmer__app__help__deployment,logs)
                cmd="wasmer__app__help__deployment__logs"
                ;;
            wasmer__app__help__region,list)
                cmd="wasmer__app__help__region__list"
                ;;
            wasmer__app__help__secret,create)
                cmd="wasmer__app__help__secret__create"
                ;;
            wasmer__app__help__secret,delete)
                cmd="wasmer__app__help__secret__delete"
                ;;
            wasmer__app__help__secret,list)
                cmd="wasmer__app__help__secret__list"
                ;;
            wasmer__app__help__secret,reveal)
                cmd="wasmer__app__help__secret__reveal"
                ;;
            wasmer__app__help__secret,update)
                cmd="wasmer__app__help__secret__update"
                ;;
            wasmer__app__help__version,activate)
                cmd="wasmer__app__help__version__activate"
                ;;
            wasmer__app__help__version,get)
                cmd="wasmer__app__help__version__get"
                ;;
            wasmer__app__help__version,list)
                cmd="wasmer__app__help__version__list"
                ;;
            wasmer__app__help__volume,credentials)
                cmd="wasmer__app__help__volume__credentials"
                ;;
            wasmer__app__help__volume,list)
                cmd="wasmer__app__help__volume__list"
                ;;
            wasmer__app__help__volume,rotate-secrets)
                cmd="wasmer__app__help__volume__rotate__secrets"
                ;;
            wasmer__app__region,help)
                cmd="wasmer__app__region__help"
                ;;
            wasmer__app__region,list)
                cmd="wasmer__app__region__list"
                ;;
            wasmer__app__region__help,help)
                cmd="wasmer__app__region__help__help"
                ;;
            wasmer__app__region__help,list)
                cmd="wasmer__app__region__help__list"
                ;;
            wasmer__app__secret,create)
                cmd="wasmer__app__secret__create"
                ;;
            wasmer__app__secret,delete)
                cmd="wasmer__app__secret__delete"
                ;;
            wasmer__app__secret,help)
                cmd="wasmer__app__secret__help"
                ;;
            wasmer__app__secret,list)
                cmd="wasmer__app__secret__list"
                ;;
            wasmer__app__secret,reveal)
                cmd="wasmer__app__secret__reveal"
                ;;
            wasmer__app__secret,update)
                cmd="wasmer__app__secret__update"
                ;;
            wasmer__app__secret__help,create)
                cmd="wasmer__app__secret__help__create"
                ;;
            wasmer__app__secret__help,delete)
                cmd="wasmer__app__secret__help__delete"
                ;;
            wasmer__app__secret__help,help)
                cmd="wasmer__app__secret__help__help"
                ;;
            wasmer__app__secret__help,list)
                cmd="wasmer__app__secret__help__list"
                ;;
            wasmer__app__secret__help,reveal)
                cmd="wasmer__app__secret__help__reveal"
                ;;
            wasmer__app__secret__help,update)
                cmd="wasmer__app__secret__help__update"
                ;;
            wasmer__app__version,activate)
                cmd="wasmer__app__version__activate"
                ;;
            wasmer__app__version,get)
                cmd="wasmer__app__version__get"
                ;;
            wasmer__app__version,help)
                cmd="wasmer__app__version__help"
                ;;
            wasmer__app__version,list)
                cmd="wasmer__app__version__list"
                ;;
            wasmer__app__version__help,activate)
                cmd="wasmer__app__version__help__activate"
                ;;
            wasmer__app__version__help,get)
                cmd="wasmer__app__version__help__get"
                ;;
            wasmer__app__version__help,help)
                cmd="wasmer__app__version__help__help"
                ;;
            wasmer__app__version__help,list)
                cmd="wasmer__app__version__help__list"
                ;;
            wasmer__app__volume,credentials)
                cmd="wasmer__app__volume__credentials"
                ;;
            wasmer__app__volume,help)
                cmd="wasmer__app__volume__help"
                ;;
            wasmer__app__volume,list)
                cmd="wasmer__app__volume__list"
                ;;
            wasmer__app__volume,rotate-secrets)
                cmd="wasmer__app__volume__rotate__secrets"
                ;;
            wasmer__app__volume__help,credentials)
                cmd="wasmer__app__volume__help__credentials"
                ;;
            wasmer__app__volume__help,help)
                cmd="wasmer__app__volume__help__help"
                ;;
            wasmer__app__volume__help,list)
                cmd="wasmer__app__volume__help__list"
                ;;
            wasmer__app__volume__help,rotate-secrets)
                cmd="wasmer__app__volume__help__rotate__secrets"
                ;;
            wasmer__auth,help)
                cmd="wasmer__auth__help"
                ;;
            wasmer__auth,login)
                cmd="wasmer__auth__login"
                ;;
            wasmer__auth,logout)
                cmd="wasmer__auth__logout"
                ;;
            wasmer__auth,whoami)
                cmd="wasmer__auth__whoami"
                ;;
            wasmer__auth__help,help)
                cmd="wasmer__auth__help__help"
                ;;
            wasmer__auth__help,login)
                cmd="wasmer__auth__help__login"
                ;;
            wasmer__auth__help,logout)
                cmd="wasmer__auth__help__logout"
                ;;
            wasmer__auth__help,whoami)
                cmd="wasmer__auth__help__whoami"
                ;;
            wasmer__binfmt,help)
                cmd="wasmer__binfmt__help"
                ;;
            wasmer__binfmt,register)
                cmd="wasmer__binfmt__register"
                ;;
            wasmer__binfmt,reregister)
                cmd="wasmer__binfmt__reregister"
                ;;
            wasmer__binfmt,unregister)
                cmd="wasmer__binfmt__unregister"
                ;;
            wasmer__binfmt__help,help)
                cmd="wasmer__binfmt__help__help"
                ;;
            wasmer__binfmt__help,register)
                cmd="wasmer__binfmt__help__register"
                ;;
            wasmer__binfmt__help,reregister)
                cmd="wasmer__binfmt__help__reregister"
                ;;
            wasmer__binfmt__help,unregister)
                cmd="wasmer__binfmt__help__unregister"
                ;;
            wasmer__cache,clean)
                cmd="wasmer__cache__clean"
                ;;
            wasmer__cache,dir)
                cmd="wasmer__cache__dir"
                ;;
            wasmer__cache,help)
                cmd="wasmer__cache__help"
                ;;
            wasmer__cache__help,clean)
                cmd="wasmer__cache__help__clean"
                ;;
            wasmer__cache__help,dir)
                cmd="wasmer__cache__help__dir"
                ;;
            wasmer__cache__help,help)
                cmd="wasmer__cache__help__help"
                ;;
            wasmer__config,get)
                cmd="wasmer__config__get"
                ;;
            wasmer__config,help)
                cmd="wasmer__config__help"
                ;;
            wasmer__config,set)
                cmd="wasmer__config__set"
                ;;
            wasmer__config__get,help)
                cmd="wasmer__config__get__help"
                ;;
            wasmer__config__get,proxy.url)
                cmd="wasmer__config__get__proxy.url"
                ;;
            wasmer__config__get,registry.token)
                cmd="wasmer__config__get__registry.token"
                ;;
            wasmer__config__get,registry.url)
                cmd="wasmer__config__get__registry.url"
                ;;
            wasmer__config__get,telemetry.enabled)
                cmd="wasmer__config__get__telemetry.enabled"
                ;;
            wasmer__config__get,update-notifications.enabled)
                cmd="wasmer__config__get__update__notifications.enabled"
                ;;
            wasmer__config__get__help,help)
                cmd="wasmer__config__get__help__help"
                ;;
            wasmer__config__get__help,proxy.url)
                cmd="wasmer__config__get__help__proxy.url"
                ;;
            wasmer__config__get__help,registry.token)
                cmd="wasmer__config__get__help__registry.token"
                ;;
            wasmer__config__get__help,registry.url)
                cmd="wasmer__config__get__help__registry.url"
                ;;
            wasmer__config__get__help,telemetry.enabled)
                cmd="wasmer__config__get__help__telemetry.enabled"
                ;;
            wasmer__config__get__help,update-notifications.enabled)
                cmd="wasmer__config__get__help__update__notifications.enabled"
                ;;
            wasmer__config__help,get)
                cmd="wasmer__config__help__get"
                ;;
            wasmer__config__help,help)
                cmd="wasmer__config__help__help"
                ;;
            wasmer__config__help,set)
                cmd="wasmer__config__help__set"
                ;;
            wasmer__config__help__get,proxy.url)
                cmd="wasmer__config__help__get__proxy.url"
                ;;
            wasmer__config__help__get,registry.token)
                cmd="wasmer__config__help__get__registry.token"
                ;;
            wasmer__config__help__get,registry.url)
                cmd="wasmer__config__help__get__registry.url"
                ;;
            wasmer__config__help__get,telemetry.enabled)
                cmd="wasmer__config__help__get__telemetry.enabled"
                ;;
            wasmer__config__help__get,update-notifications.enabled)
                cmd="wasmer__config__help__get__update__notifications.enabled"
                ;;
            wasmer__config__help__set,proxy.url)
                cmd="wasmer__config__help__set__proxy.url"
                ;;
            wasmer__config__help__set,registry.token)
                cmd="wasmer__config__help__set__registry.token"
                ;;
            wasmer__config__help__set,registry.url)
                cmd="wasmer__config__help__set__registry.url"
                ;;
            wasmer__config__help__set,telemetry.enabled)
                cmd="wasmer__config__help__set__telemetry.enabled"
                ;;
            wasmer__config__help__set,update-notifications.enabled)
                cmd="wasmer__config__help__set__update__notifications.enabled"
                ;;
            wasmer__config__set,help)
                cmd="wasmer__config__set__help"
                ;;
            wasmer__config__set,proxy.url)
                cmd="wasmer__config__set__proxy.url"
                ;;
            wasmer__config__set,registry.token)
                cmd="wasmer__config__set__registry.token"
                ;;
            wasmer__config__set,registry.url)
                cmd="wasmer__config__set__registry.url"
                ;;
            wasmer__config__set,telemetry.enabled)
                cmd="wasmer__config__set__telemetry.enabled"
                ;;
            wasmer__config__set,update-notifications.enabled)
                cmd="wasmer__config__set__update__notifications.enabled"
                ;;
            wasmer__config__set__help,help)
                cmd="wasmer__config__set__help__help"
                ;;
            wasmer__config__set__help,proxy.url)
                cmd="wasmer__config__set__help__proxy.url"
                ;;
            wasmer__config__set__help,registry.token)
                cmd="wasmer__config__set__help__registry.token"
                ;;
            wasmer__config__set__help,registry.url)
                cmd="wasmer__config__set__help__registry.url"
                ;;
            wasmer__config__set__help,telemetry.enabled)
                cmd="wasmer__config__set__help__telemetry.enabled"
                ;;
            wasmer__config__set__help,update-notifications.enabled)
                cmd="wasmer__config__set__help__update__notifications.enabled"
                ;;
            wasmer__container,help)
                cmd="wasmer__container__help"
                ;;
            wasmer__container,unpack)
                cmd="wasmer__container__unpack"
                ;;
            wasmer__container__help,help)
                cmd="wasmer__container__help__help"
                ;;
            wasmer__container__help,unpack)
                cmd="wasmer__container__help__unpack"
                ;;
            wasmer__domain,get)
                cmd="wasmer__domain__get"
                ;;
            wasmer__domain,get-zone-file)
                cmd="wasmer__domain__get__zone__file"
                ;;
            wasmer__domain,help)
                cmd="wasmer__domain__help"
                ;;
            wasmer__domain,list)
                cmd="wasmer__domain__list"
                ;;
            wasmer__domain,register)
                cmd="wasmer__domain__register"
                ;;
            wasmer__domain,sync-zone-file)
                cmd="wasmer__domain__sync__zone__file"
                ;;
            wasmer__domain__help,get)
                cmd="wasmer__domain__help__get"
                ;;
            wasmer__domain__help,get-zone-file)
                cmd="wasmer__domain__help__get__zone__file"
                ;;
            wasmer__domain__help,help)
                cmd="wasmer__domain__help__help"
                ;;
            wasmer__domain__help,list)
                cmd="wasmer__domain__help__list"
                ;;
            wasmer__domain__help,register)
                cmd="wasmer__domain__help__register"
                ;;
            wasmer__domain__help,sync-zone-file)
                cmd="wasmer__domain__help__sync__zone__file"
                ;;
            wasmer__help,add)
                cmd="wasmer__help__add"
                ;;
            wasmer__help,app)
                cmd="wasmer__help__app"
                ;;
            wasmer__help,auth)
                cmd="wasmer__help__auth"
                ;;
            wasmer__help,binfmt)
                cmd="wasmer__help__binfmt"
                ;;
            wasmer__help,cache)
                cmd="wasmer__help__cache"
                ;;
            wasmer__help,compile)
                cmd="wasmer__help__compile"
                ;;
            wasmer__help,config)
                cmd="wasmer__help__config"
                ;;
            wasmer__help,container)
                cmd="wasmer__help__container"
                ;;
            wasmer__help,create-exe)
                cmd="wasmer__help__create__exe"
                ;;
            wasmer__help,create-obj)
                cmd="wasmer__help__create__obj"
                ;;
            wasmer__help,deploy)
                cmd="wasmer__help__deploy"
                ;;
            wasmer__help,domain)
                cmd="wasmer__help__domain"
                ;;
            wasmer__help,gen-c-header)
                cmd="wasmer__help__gen__c__header"
                ;;
            wasmer__help,gen-completions)
                cmd="wasmer__help__gen__completions"
                ;;
            wasmer__help,gen-man)
                cmd="wasmer__help__gen__man"
                ;;
            wasmer__help,help)
                cmd="wasmer__help__help"
                ;;
            wasmer__help,init)
                cmd="wasmer__help__init"
                ;;
            wasmer__help,inspect)
                cmd="wasmer__help__inspect"
                ;;
            wasmer__help,journal)
                cmd="wasmer__help__journal"
                ;;
            wasmer__help,login)
                cmd="wasmer__help__login"
                ;;
            wasmer__help,namespace)
                cmd="wasmer__help__namespace"
                ;;
            wasmer__help,package)
                cmd="wasmer__help__package"
                ;;
            wasmer__help,publish)
                cmd="wasmer__help__publish"
                ;;
            wasmer__help,run)
                cmd="wasmer__help__run"
                ;;
            wasmer__help,self-update)
                cmd="wasmer__help__self__update"
                ;;
            wasmer__help,ssh)
                cmd="wasmer__help__ssh"
                ;;
            wasmer__help,validate)
                cmd="wasmer__help__validate"
                ;;
            wasmer__help,wast)
                cmd="wasmer__help__wast"
                ;;
            wasmer__help,whoami)
                cmd="wasmer__help__whoami"
                ;;
            wasmer__help__app,create)
                cmd="wasmer__help__app__create"
                ;;
            wasmer__help__app,database)
                cmd="wasmer__help__app__database"
                ;;
            wasmer__help__app,delete)
                cmd="wasmer__help__app__delete"
                ;;
            wasmer__help__app,deploy)
                cmd="wasmer__help__app__deploy"
                ;;
            wasmer__help__app,deployment)
                cmd="wasmer__help__app__deployment"
                ;;
            wasmer__help__app,get)
                cmd="wasmer__help__app__get"
                ;;
            wasmer__help__app,info)
                cmd="wasmer__help__app__info"
                ;;
            wasmer__help__app,list)
                cmd="wasmer__help__app__list"
                ;;
            wasmer__help__app,logs)
                cmd="wasmer__help__app__logs"
                ;;
            wasmer__help__app,purge-cache)
                cmd="wasmer__help__app__purge__cache"
                ;;
            wasmer__help__app,region)
                cmd="wasmer__help__app__region"
                ;;
            wasmer__help__app,secret)
                cmd="wasmer__help__app__secret"
                ;;
            wasmer__help__app,version)
                cmd="wasmer__help__app__version"
                ;;
            wasmer__help__app,volume)
                cmd="wasmer__help__app__volume"
                ;;
            wasmer__help__app__database,list)
                cmd="wasmer__help__app__database__list"
                ;;
            wasmer__help__app__deployment,get)
                cmd="wasmer__help__app__deployment__get"
                ;;
            wasmer__help__app__deployment,list)
                cmd="wasmer__help__app__deployment__list"
                ;;
            wasmer__help__app__deployment,logs)
                cmd="wasmer__help__app__deployment__logs"
                ;;
            wasmer__help__app__region,list)
                cmd="wasmer__help__app__region__list"
                ;;
            wasmer__help__app__secret,create)
                cmd="wasmer__help__app__secret__create"
                ;;
            wasmer__help__app__secret,delete)
                cmd="wasmer__help__app__secret__delete"
                ;;
            wasmer__help__app__secret,list)
                cmd="wasmer__help__app__secret__list"
                ;;
            wasmer__help__app__secret,reveal)
                cmd="wasmer__help__app__secret__reveal"
                ;;
            wasmer__help__app__secret,update)
                cmd="wasmer__help__app__secret__update"
                ;;
            wasmer__help__app__version,activate)
                cmd="wasmer__help__app__version__activate"
                ;;
            wasmer__help__app__version,get)
                cmd="wasmer__help__app__version__get"
                ;;
            wasmer__help__app__version,list)
                cmd="wasmer__help__app__version__list"
                ;;
            wasmer__help__app__volume,credentials)
                cmd="wasmer__help__app__volume__credentials"
                ;;
            wasmer__help__app__volume,list)
                cmd="wasmer__help__app__volume__list"
                ;;
            wasmer__help__app__volume,rotate-secrets)
                cmd="wasmer__help__app__volume__rotate__secrets"
                ;;
            wasmer__help__auth,login)
                cmd="wasmer__help__auth__login"
                ;;
            wasmer__help__auth,logout)
                cmd="wasmer__help__auth__logout"
                ;;
            wasmer__help__auth,whoami)
                cmd="wasmer__help__auth__whoami"
                ;;
            wasmer__help__binfmt,register)
                cmd="wasmer__help__binfmt__register"
                ;;
            wasmer__help__binfmt,reregister)
                cmd="wasmer__help__binfmt__reregister"
                ;;
            wasmer__help__binfmt,unregister)
                cmd="wasmer__help__binfmt__unregister"
                ;;
            wasmer__help__cache,clean)
                cmd="wasmer__help__cache__clean"
                ;;
            wasmer__help__cache,dir)
                cmd="wasmer__help__cache__dir"
                ;;
            wasmer__help__config,get)
                cmd="wasmer__help__config__get"
                ;;
            wasmer__help__config,set)
                cmd="wasmer__help__config__set"
                ;;
            wasmer__help__config__get,proxy.url)
                cmd="wasmer__help__config__get__proxy.url"
                ;;
            wasmer__help__config__get,registry.token)
                cmd="wasmer__help__config__get__registry.token"
                ;;
            wasmer__help__config__get,registry.url)
                cmd="wasmer__help__config__get__registry.url"
                ;;
            wasmer__help__config__get,telemetry.enabled)
                cmd="wasmer__help__config__get__telemetry.enabled"
                ;;
            wasmer__help__config__get,update-notifications.enabled)
                cmd="wasmer__help__config__get__update__notifications.enabled"
                ;;
            wasmer__help__config__set,proxy.url)
                cmd="wasmer__help__config__set__proxy.url"
                ;;
            wasmer__help__config__set,registry.token)
                cmd="wasmer__help__config__set__registry.token"
                ;;
            wasmer__help__config__set,registry.url)
                cmd="wasmer__help__config__set__registry.url"
                ;;
            wasmer__help__config__set,telemetry.enabled)
                cmd="wasmer__help__config__set__telemetry.enabled"
                ;;
            wasmer__help__config__set,update-notifications.enabled)
                cmd="wasmer__help__config__set__update__notifications.enabled"
                ;;
            wasmer__help__container,unpack)
                cmd="wasmer__help__container__unpack"
                ;;
            wasmer__help__domain,get)
                cmd="wasmer__help__domain__get"
                ;;
            wasmer__help__domain,get-zone-file)
                cmd="wasmer__help__domain__get__zone__file"
                ;;
            wasmer__help__domain,list)
                cmd="wasmer__help__domain__list"
                ;;
            wasmer__help__domain,register)
                cmd="wasmer__help__domain__register"
                ;;
            wasmer__help__domain,sync-zone-file)
                cmd="wasmer__help__domain__sync__zone__file"
                ;;
            wasmer__help__journal,compact)
                cmd="wasmer__help__journal__compact"
                ;;
            wasmer__help__journal,export)
                cmd="wasmer__help__journal__export"
                ;;
            wasmer__help__journal,extract)
                cmd="wasmer__help__journal__extract"
                ;;
            wasmer__help__journal,filter)
                cmd="wasmer__help__journal__filter"
                ;;
            wasmer__help__journal,import)
                cmd="wasmer__help__journal__import"
                ;;
            wasmer__help__journal,inspect)
                cmd="wasmer__help__journal__inspect"
                ;;
            wasmer__help__journal__extract,memory)
                cmd="wasmer__help__journal__extract__memory"
                ;;
            wasmer__help__namespace,get)
                cmd="wasmer__help__namespace__get"
                ;;
            wasmer__help__namespace,list)
                cmd="wasmer__help__namespace__list"
                ;;
            wasmer__help__package,build)
                cmd="wasmer__help__package__build"
                ;;
            wasmer__help__package,download)
                cmd="wasmer__help__package__download"
                ;;
            wasmer__help__package,publish)
                cmd="wasmer__help__package__publish"
                ;;
            wasmer__help__package,push)
                cmd="wasmer__help__package__push"
                ;;
            wasmer__help__package,tag)
                cmd="wasmer__help__package__tag"
                ;;
            wasmer__help__package,unpack)
                cmd="wasmer__help__package__unpack"
                ;;
            wasmer__journal,compact)
                cmd="wasmer__journal__compact"
                ;;
            wasmer__journal,export)
                cmd="wasmer__journal__export"
                ;;
            wasmer__journal,extract)
                cmd="wasmer__journal__extract"
                ;;
            wasmer__journal,filter)
                cmd="wasmer__journal__filter"
                ;;
            wasmer__journal,help)
                cmd="wasmer__journal__help"
                ;;
            wasmer__journal,import)
                cmd="wasmer__journal__import"
                ;;
            wasmer__journal,inspect)
                cmd="wasmer__journal__inspect"
                ;;
            wasmer__journal__extract,help)
                cmd="wasmer__journal__extract__help"
                ;;
            wasmer__journal__extract,memory)
                cmd="wasmer__journal__extract__memory"
                ;;
            wasmer__journal__extract__help,help)
                cmd="wasmer__journal__extract__help__help"
                ;;
            wasmer__journal__extract__help,memory)
                cmd="wasmer__journal__extract__help__memory"
                ;;
            wasmer__journal__help,compact)
                cmd="wasmer__journal__help__compact"
                ;;
            wasmer__journal__help,export)
                cmd="wasmer__journal__help__export"
                ;;
            wasmer__journal__help,extract)
                cmd="wasmer__journal__help__extract"
                ;;
            wasmer__journal__help,filter)
                cmd="wasmer__journal__help__filter"
                ;;
            wasmer__journal__help,help)
                cmd="wasmer__journal__help__help"
                ;;
            wasmer__journal__help,import)
                cmd="wasmer__journal__help__import"
                ;;
            wasmer__journal__help,inspect)
                cmd="wasmer__journal__help__inspect"
                ;;
            wasmer__journal__help__extract,memory)
                cmd="wasmer__journal__help__extract__memory"
                ;;
            wasmer__namespace,get)
                cmd="wasmer__namespace__get"
                ;;
            wasmer__namespace,help)
                cmd="wasmer__namespace__help"
                ;;
            wasmer__namespace,list)
                cmd="wasmer__namespace__list"
                ;;
            wasmer__namespace__help,get)
                cmd="wasmer__namespace__help__get"
                ;;
            wasmer__namespace__help,help)
                cmd="wasmer__namespace__help__help"
                ;;
            wasmer__namespace__help,list)
                cmd="wasmer__namespace__help__list"
                ;;
            wasmer__package,build)
                cmd="wasmer__package__build"
                ;;
            wasmer__package,download)
                cmd="wasmer__package__download"
                ;;
            wasmer__package,help)
                cmd="wasmer__package__help"
                ;;
            wasmer__package,publish)
                cmd="wasmer__package__publish"
                ;;
            wasmer__package,push)
                cmd="wasmer__package__push"
                ;;
            wasmer__package,tag)
                cmd="wasmer__package__tag"
                ;;
            wasmer__package,unpack)
                cmd="wasmer__package__unpack"
                ;;
            wasmer__package__help,build)
                cmd="wasmer__package__help__build"
                ;;
            wasmer__package__help,download)
                cmd="wasmer__package__help__download"
                ;;
            wasmer__package__help,help)
                cmd="wasmer__package__help__help"
                ;;
            wasmer__package__help,publish)
                cmd="wasmer__package__help__publish"
                ;;
            wasmer__package__help,push)
                cmd="wasmer__package__help__push"
                ;;
            wasmer__package__help,tag)
                cmd="wasmer__package__help__tag"
                ;;
            wasmer__package__help,unpack)
                cmd="wasmer__package__help__unpack"
                ;;
            *)
                ;;
        esac
    done

    case "${cmd}" in
        wasmer)
            opts="-V -v -q -h --version --verbose --quiet --log-format --log-events --color --help login auth publish cache validate compile create-exe create-obj gen-c-header config self-update inspect init wast binfmt whoami add run journal package container deploy app ssh namespace domain gen-completions gen-man help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 1 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__add)
            opts="-v -q -h --wasmer-dir --cache-dir --registry --token --npm --yarn --pnpm --dev --pip --verbose --quiet --log-format --log-events --color --help [PACKAGES]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help deploy create get info list logs purge-cache delete version secret region volume database deployment help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__create)
            opts="-f -v -h --template --package --use-local-manifest --deploy --no-validate --non-interactive --offline --owner --name --dir --no-wait --wasmer-dir --cache-dir --registry --token --format --new-package-name --quiet --verbose --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --template)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --owner)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --name)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --new-package-name)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__database)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help list help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__database__help)
            opts="list help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__database__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__database__help__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__database__list)
            opts="-f -v -q -h --format --wasmer-dir --cache-dir --registry --token --with-password --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__delete)
            opts="-v -q -h --wasmer-dir --cache-dir --registry --token --non-interactive --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__deploy)
            opts="-f -v -h --wasmer-dir --cache-dir --registry --token --format --no-validate --non-interactive --publish-package --dir --path --no-wait --no-default --no-persist-id --owner --app-name --bump --quiet --template --package --use-local-manifest --verbose --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --path)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --owner)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app-name)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --template)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__deployment)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help list get logs help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__deployment__get)
            opts="-f -v -q -h --format --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help <ID>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__deployment__help)
            opts="list get logs help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__deployment__help__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__deployment__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__deployment__help__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__deployment__help__logs)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__deployment__list)
            opts="-f -v -q -h --format --wasmer-dir --cache-dir --registry --token --offset --limit --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --offset)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --limit)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__deployment__logs)
            opts="-f -v -q -h --format --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help <ID>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__get)
            opts="-f -v -q -h --wasmer-dir --cache-dir --registry --token --format --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help)
            opts="deploy create get info list logs purge-cache delete version secret region volume database deployment help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__create)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__database)
            opts="list"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__database__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__delete)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__deploy)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__deployment)
            opts="list get logs"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__deployment__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__deployment__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__deployment__logs)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__info)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__logs)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__purge__cache)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__region)
            opts="list"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__region__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__secret)
            opts="create delete reveal list update"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__secret__create)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__secret__delete)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__secret__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__secret__reveal)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__secret__update)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__version)
            opts="get list activate"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__version__activate)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__version__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__version__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__volume)
            opts="credentials list rotate-secrets"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__volume__credentials)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__volume__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__help__volume__rotate__secrets)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__info)
            opts="-v -q -h --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__list)
            opts="-f -n -a -v -q -h --format --wasmer-dir --cache-dir --registry --token --namespace --all --max --paging-mode --sort --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --namespace)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -n)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --max)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --sort)
                    COMPREPLY=($(compgen -W "newest oldest last-updated" -- "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__logs)
            opts="-f -v -q -h --wasmer-dir --cache-dir --registry --token --format --from --until --max --watch --streams --request-id --instance-id --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --from)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --until)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --max)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --streams)
                    COMPREPLY=($(compgen -W "stdout stderr" -- "${cur}"))
                    return 0
                    ;;
                --request-id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --instance-id)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__purge__cache)
            opts="-f -v -q -h --wasmer-dir --cache-dir --registry --token --format --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__region)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help list help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__region__help)
            opts="list help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__region__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__region__help__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__region__list)
            opts="-f -v -h --wasmer-dir --cache-dir --registry --token --quiet --non-interactive --format --verbose --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help create delete reveal list update help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__create)
            opts="-v -h --wasmer-dir --cache-dir --registry --token --quiet --non-interactive --app-dir --app --from-file --redeploy --verbose --log-format --log-events --color --help [name] [value]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --from-file)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__delete)
            opts="-v -h --wasmer-dir --cache-dir --registry --token --quiet --non-interactive --app --app-dir --from-file --all --force --verbose --log-format --log-events --color --help [name]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --from-file)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__help)
            opts="create delete reveal list update help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__help__create)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__help__delete)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__help__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__help__reveal)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__help__update)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__list)
            opts="-f -v -h --wasmer-dir --cache-dir --registry --token --quiet --non-interactive --format --app --app-dir --verbose --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__reveal)
            opts="-f -v -h --wasmer-dir --cache-dir --registry --token --quiet --non-interactive --format --app --app-dir --all --verbose --log-format --log-events --color --help [name]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__secret__update)
            opts="-v -h --wasmer-dir --cache-dir --registry --token --quiet --non-interactive --app-dir --app --from-file --redeploy --verbose --log-format --log-events --color --help [name] [value]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --from-file)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__version)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help get list activate help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__version__activate)
            opts="-f -v -q -h --wasmer-dir --cache-dir --registry --token --format --verbose --quiet --log-format --log-events --color --help <VERSION>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__version__get)
            opts="-f -v -q -h --wasmer-dir --cache-dir --registry --token --format --name --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --name)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__version__help)
            opts="get list activate help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__version__help__activate)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__version__help__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__version__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__version__help__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__version__list)
            opts="-f -a -v -q -h --wasmer-dir --cache-dir --registry --token --format --all --offset --max --before --after --sort --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --offset)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --max)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --before)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --after)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --sort)
                    COMPREPLY=($(compgen -W "newest oldest" -- "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__volume)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help credentials list rotate-secrets help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__volume__credentials)
            opts="-f -v -q -h --wasmer-dir --cache-dir --registry --token --format --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -W "json yaml table rclone" -- "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -W "json yaml table rclone" -- "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__volume__help)
            opts="credentials list rotate-secrets help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__volume__help__credentials)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__volume__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__volume__help__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__volume__help__rotate__secrets)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__volume__list)
            opts="-f -v -q -h --format --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__app__volume__rotate__secrets)
            opts="-f -v -q -h --wasmer-dir --cache-dir --registry --token --format --verbose --quiet --log-format --log-events --color --help [APP]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -W "json yaml table rclone" -- "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -W "json yaml table rclone" -- "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__auth)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help login logout whoami help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__auth__help)
            opts="login logout whoami help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__auth__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__auth__help__login)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__auth__help__logout)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__auth__help__whoami)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__auth__login)
            opts="-v -q -h --no-browser --wasmer-dir --cache-dir --registry --verbose --quiet --log-format --log-events --color --help [TOKEN]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__auth__logout)
            opts="-v -q -h --wasmer-dir --cache-dir --registry --token --non-interactive --revoke-token --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__auth__whoami)
            opts="-v -q -h --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__binfmt)
            opts="-v -q -h --binfmt-misc --verbose --quiet --log-format --log-events --color --help register unregister reregister help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --binfmt-misc)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__binfmt__help)
            opts="register unregister reregister help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__binfmt__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__binfmt__help__register)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__binfmt__help__reregister)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__binfmt__help__unregister)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__binfmt__register)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__binfmt__reregister)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__binfmt__unregister)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__cache)
            opts="-v -q -h --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help clean dir help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__cache__clean)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__cache__dir)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__cache__help)
            opts="clean dir help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__cache__help__clean)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__cache__help__dir)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__cache__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__compile)
            opts="-o -m -v -q -h --target --singlepass --cranelift --llvm --v8 --wamr --wasmi --enable-verifier --profiler --llvm-debug-dir --enable-pass-params-opt --llvm-num-threads --enable-simd --disable-threads --enable-threads --enable-reference-types --enable-multi-value --enable-bulk-memory --enable-tail-call --enable-module-linking --enable-multi-memory --enable-memory64 --enable-exceptions --enable-relaxed-simd --enable-extended-const --enable-all --hash-algorithm --verbose --quiet --log-format --log-events --color --help <FILE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                -o)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --target)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --profiler)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-debug-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-num-threads)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -m)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --hash-algorithm)
                    COMPREPLY=($(compgen -W "sha256 xx-hash" -- "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config)
            opts="-v -q -h --wasmer-dir --cache-dir --registry --token --prefix --bindir --includedir --libdir --libs --cflags --config-path --pkg-config --verbose --quiet --log-format --log-events --color --help get set help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help registry.url registry.token telemetry.enabled update-notifications.enabled proxy.url help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__help)
            opts="registry.url registry.token telemetry.enabled update-notifications.enabled proxy.url help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__help__proxy.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__help__registry.token)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__help__registry.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__help__telemetry.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__help__update__notifications.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__proxy.url)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__registry.token)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__registry.url)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__telemetry.enabled)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__get__update__notifications.enabled)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help)
            opts="get set help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__get)
            opts="registry.url registry.token telemetry.enabled update-notifications.enabled proxy.url"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__get__proxy.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__get__registry.token)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__get__registry.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__get__telemetry.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__get__update__notifications.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__set)
            opts="registry.url registry.token telemetry.enabled update-notifications.enabled proxy.url"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__set__proxy.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__set__registry.token)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__set__registry.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__set__telemetry.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__help__set__update__notifications.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help registry.url registry.token telemetry.enabled update-notifications.enabled proxy.url help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__help)
            opts="registry.url registry.token telemetry.enabled update-notifications.enabled proxy.url help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__help__proxy.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__help__registry.token)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__help__registry.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__help__telemetry.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__help__update__notifications.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__proxy.url)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <URL>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__registry.token)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <TOKEN>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__registry.url)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <URL>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__telemetry.enabled)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <ENABLED>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__config__set__update__notifications.enabled)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <ENABLED>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__container)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help unpack help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__container__help)
            opts="unpack help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__container__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__container__help__unpack)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__container__unpack)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__create__exe)
            opts="-o -m -l -v -q -h --wasmer-dir --cache-dir --registry --token --debug-dir --precompiled-atom --target --use-wasmer-release --cpu-features --libraries --use-system-linker --library-path --tarball --zig-binary-path --singlepass --cranelift --llvm --v8 --wamr --wasmi --enable-verifier --profiler --llvm-debug-dir --enable-pass-params-opt --llvm-num-threads --enable-simd --disable-threads --enable-threads --enable-reference-types --enable-multi-value --enable-bulk-memory --enable-tail-call --enable-module-linking --enable-multi-memory --enable-memory64 --enable-exceptions --enable-relaxed-simd --enable-extended-const --enable-all --hash-algorithm --verbose --quiet --log-format --log-events --color --help <FILE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -o)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --debug-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --precompiled-atom)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --target)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --use-wasmer-release)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cpu-features)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -m)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --libraries)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -l)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --library-path)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --tarball)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --zig-binary-path)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --profiler)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-debug-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-num-threads)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --hash-algorithm)
                    COMPREPLY=($(compgen -W "sha256 xx-hash" -- "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__create__obj)
            opts="-o -m -v -q -h --debug-dir --prefix --atom --target --cpu-features --singlepass --cranelift --llvm --v8 --wamr --wasmi --enable-verifier --profiler --llvm-debug-dir --enable-pass-params-opt --llvm-num-threads --enable-simd --disable-threads --enable-threads --enable-reference-types --enable-multi-value --enable-bulk-memory --enable-tail-call --enable-module-linking --enable-multi-memory --enable-memory64 --enable-exceptions --enable-relaxed-simd --enable-extended-const --enable-all --verbose --quiet --log-format --log-events --color --help <FILE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                -o)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --debug-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --prefix)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --atom)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --target)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cpu-features)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -m)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --profiler)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-debug-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-num-threads)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__deploy)
            opts="-f -v -h --wasmer-dir --cache-dir --registry --token --format --no-validate --non-interactive --publish-package --dir --path --no-wait --no-default --no-persist-id --owner --app-name --bump --quiet --template --package --use-local-manifest --verbose --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --path)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --owner)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --app-name)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --template)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --package)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help list get get-zone-file sync-zone-file register help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__get)
            opts="-f -v -q -h --format --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help <NAME>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__get__zone__file)
            opts="-f -o -v -q -h --format --wasmer-dir --cache-dir --registry --token --output --verbose --quiet --log-format --log-events --color --help <DOMAIN_NAME>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --output)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -o)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__help)
            opts="list get get-zone-file sync-zone-file register help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__help__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__help__get__zone__file)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__help__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__help__register)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__help__sync__zone__file)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__list)
            opts="-f -v -q -h --format --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help [NAMESPACE]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__register)
            opts="-f -n -i -v -q -h --format --wasmer-dir --cache-dir --registry --token --namespace --import-records --verbose --quiet --log-format --log-events --color --help <NAME>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --namespace)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -n)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__domain__sync__zone__file)
            opts="-n -v -q -h --wasmer-dir --cache-dir --registry --token --no-delete-missing-records --verbose --quiet --log-format --log-events --color --help <ZONE_FILE_PATH>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__gen__c__header)
            opts="-o -m -v -q -h --prefix --atom --target --cpu-features --verbose --quiet --log-format --log-events --color --help <FILE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --prefix)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --atom)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -o)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --target)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cpu-features)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -m)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__gen__completions)
            opts="-v -q -h --out --verbose --quiet --log-format --log-events --color --help bash elvish fish powershell zsh"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --out)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__gen__man)
            opts="-v -q -h --out --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --out)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help)
            opts="login auth publish cache validate compile create-exe create-obj gen-c-header config self-update inspect init wast binfmt whoami add run journal package container deploy app ssh namespace domain gen-completions gen-man help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__add)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app)
            opts="deploy create get info list logs purge-cache delete version secret region volume database deployment"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__create)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__database)
            opts="list"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__database__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__delete)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__deploy)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__deployment)
            opts="list get logs"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__deployment__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__deployment__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__deployment__logs)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__info)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__logs)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__purge__cache)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__region)
            opts="list"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__region__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__secret)
            opts="create delete reveal list update"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__secret__create)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__secret__delete)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__secret__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__secret__reveal)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__secret__update)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__version)
            opts="get list activate"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__version__activate)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__version__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__version__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__volume)
            opts="credentials list rotate-secrets"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__volume__credentials)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__volume__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__app__volume__rotate__secrets)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__auth)
            opts="login logout whoami"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__auth__login)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__auth__logout)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__auth__whoami)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__binfmt)
            opts="register unregister reregister"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__binfmt__register)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__binfmt__reregister)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__binfmt__unregister)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__cache)
            opts="clean dir"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__cache__clean)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__cache__dir)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__compile)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config)
            opts="get set"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__get)
            opts="registry.url registry.token telemetry.enabled update-notifications.enabled proxy.url"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__get__proxy.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__get__registry.token)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__get__registry.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__get__telemetry.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__get__update__notifications.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__set)
            opts="registry.url registry.token telemetry.enabled update-notifications.enabled proxy.url"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__set__proxy.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__set__registry.token)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__set__registry.url)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__set__telemetry.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__config__set__update__notifications.enabled)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__container)
            opts="unpack"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__container__unpack)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__create__exe)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__create__obj)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__deploy)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__domain)
            opts="list get get-zone-file sync-zone-file register"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__domain__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__domain__get__zone__file)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__domain__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__domain__register)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__domain__sync__zone__file)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__gen__c__header)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__gen__completions)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__gen__man)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__init)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__inspect)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__journal)
            opts="compact export import inspect filter extract"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__journal__compact)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__journal__export)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__journal__extract)
            opts="memory"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__journal__extract__memory)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__journal__filter)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__journal__import)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__journal__inspect)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__login)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__namespace)
            opts="get list"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__namespace__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__namespace__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__package)
            opts="download build tag push publish unpack"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__package__build)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__package__download)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__package__publish)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__package__push)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__package__tag)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__package__unpack)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__publish)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__run)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__self__update)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__ssh)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__validate)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__wast)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__help__whoami)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__init)
            opts="-v -h --wasmer-dir --cache-dir --registry --token --lib --bin --empty --overwrite --quiet --namespace --package-name --version --manifest-path --template --include --verbose --log-format --log-events --color --help [PACKAGE_PATH]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --namespace)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --package-name)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --version)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --manifest-path)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --template)
                    COMPREPLY=($(compgen -W "python js" -- "${cur}"))
                    return 0
                    ;;
                --include)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__inspect)
            opts="-v -q -h --singlepass --cranelift --llvm --v8 --wamr --wasmi --enable-verifier --profiler --llvm-debug-dir --enable-pass-params-opt --llvm-num-threads --enable-simd --disable-threads --enable-threads --enable-reference-types --enable-multi-value --enable-bulk-memory --enable-tail-call --enable-module-linking --enable-multi-memory --enable-memory64 --enable-exceptions --enable-relaxed-simd --enable-extended-const --enable-all --verbose --quiet --log-format --log-events --color --help <FILE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --profiler)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-debug-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-num-threads)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help compact export import inspect filter extract help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__compact)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <JOURNAL_PATH>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__export)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <JOURNAL_PATH>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__extract)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <JOURNAL_PATH> memory help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__extract__help)
            opts="memory help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__extract__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__extract__help__memory)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__extract__memory)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <MEMORY_FILE_PATH>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__filter)
            opts="-f -v -q -h --filter --verbose --quiet --log-format --log-events --color --help <SOURCE_PATH> <TARGET_PATH>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --filter)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__help)
            opts="compact export import inspect filter extract help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__help__compact)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__help__export)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__help__extract)
            opts="memory"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__help__extract__memory)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 5 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__help__filter)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__help__import)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__help__inspect)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__import)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <JOURNAL_PATH>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__journal__inspect)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help <JOURNAL_PATH>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__login)
            opts="-v -q -h --no-browser --wasmer-dir --cache-dir --registry --verbose --quiet --log-format --log-events --color --help [TOKEN]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__namespace)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help get list help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__namespace__get)
            opts="-f -v -q -h --format --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help <NAME>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__namespace__help)
            opts="get list help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__namespace__help__get)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__namespace__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__namespace__help__list)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__namespace__list)
            opts="-f -v -q -h --format --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --format)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help download build tag push publish unpack help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__build)
            opts="-o -v -h --out --quiet --check --verbose --log-format --log-events --color --help [PACKAGE]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --out)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -o)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__download)
            opts="-o -v -h --wasmer-dir --cache-dir --registry --token --validate --out-path --quiet --unpack --verbose --log-format --log-events --color --help <PACKAGE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --out-path)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -o)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__help)
            opts="download build tag push publish unpack help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__help__build)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__help__download)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__help__help)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__help__publish)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__help__push)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__help__tag)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__help__unpack)
            opts=""
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__publish)
            opts="-v -h --wasmer-dir --cache-dir --registry --token --dry-run --quiet --namespace --name --version --no-validate --wait --timeout --bump --non-interactive --verbose --log-format --log-events --color --help [path]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --namespace)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --name)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --version)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wait)
                    COMPREPLY=($(compgen -W "none container native-executables bindings all" -- "${cur}"))
                    return 0
                    ;;
                --timeout)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__push)
            opts="-v -h --wasmer-dir --cache-dir --registry --token --dry-run --quiet --namespace --name --timeout --non-interactive --verbose --log-format --log-events --color --help [path]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --namespace)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --name)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --timeout)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__tag)
            opts="-v -h --wasmer-dir --cache-dir --registry --token --dry-run --quiet --namespace --name --version --timeout --bump --non-interactive --path --wait --verbose --log-format --log-events --color --help <hash> [package_ident]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --namespace)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --name)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --version)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --timeout)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --path)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wait)
                    COMPREPLY=($(compgen -W "none container native-executables bindings all" -- "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__package__unpack)
            opts="-o -f -v -h --out-dir --overwrite --quiet --format --verbose --log-format --log-events --color --help <PACKAGE_PATH>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --out-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -o)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --format)
                    COMPREPLY=($(compgen -W "package webc" -- "${cur}"))
                    return 0
                    ;;
                -f)
                    COMPREPLY=($(compgen -W "package webc" -- "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__publish)
            opts="-v -h --wasmer-dir --cache-dir --registry --token --dry-run --quiet --namespace --name --version --no-validate --wait --timeout --bump --non-interactive --verbose --log-format --log-events --color --help [path]"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --namespace)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --name)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --version)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --wait)
                    COMPREPLY=($(compgen -W "none container native-executables bindings all" -- "${cur}"))
                    return 0
                    ;;
                --timeout)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__run)
            opts="-a -e -i -v -q -h --wasmer-dir --cache-dir --registry --token --singlepass --cranelift --llvm --v8 --wamr --wasmi --enable-verifier --profiler --llvm-debug-dir --enable-pass-params-opt --llvm-num-threads --enable-simd --disable-threads --enable-threads --enable-reference-types --enable-multi-value --enable-bulk-memory --enable-tail-call --enable-module-linking --enable-multi-memory --enable-memory64 --enable-exceptions --enable-relaxed-simd --enable-extended-const --enable-all --dir --mapdir --env --forward-host-env --use --include-webc --map-command --net --no-tty --enable-async-threads --enable-cpu-backoff --journal --journal-writable --enable-compaction --without-compact-on-drop --with-compact-on-growth --snapshot-on --snapshot-period --stop-after-snapshot --skip-journal-stdio --http-client --deny-multiple-wasi-versions --addr --stack-size --entrypoint --invoke --COREDUMP_PATH --hash-algorithm --verbose --quiet --log-format --log-events --color --help <INPUT> [ARGS]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --profiler)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-debug-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-num-threads)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --mapdir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --env)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --use)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --include-webc)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --map-command)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --net)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --enable-cpu-backoff)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --journal)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --journal-writable)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --with-compact-on-growth)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --snapshot-on)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --snapshot-period)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --addr)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -a)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --stack-size)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --entrypoint)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -e)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --invoke)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                -i)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --COREDUMP_PATH)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --hash-algorithm)
                    COMPREPLY=($(compgen -W "sha256 xx-hash" -- "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__self__update)
            opts="-v -q -h --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__ssh)
            opts="-p -v -q -h --wasmer-dir --cache-dir --registry --token --ssh-port --map-port --host --print --verbose --quiet --log-format --log-events --color --help [RUN] [RUN_ARGS]..."
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --ssh-port)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --map-port)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --host)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__validate)
            opts="-v -q -h --singlepass --cranelift --llvm --v8 --wamr --wasmi --enable-verifier --profiler --llvm-debug-dir --enable-pass-params-opt --llvm-num-threads --enable-simd --disable-threads --enable-threads --enable-reference-types --enable-multi-value --enable-bulk-memory --enable-tail-call --enable-module-linking --enable-multi-memory --enable-memory64 --enable-exceptions --enable-relaxed-simd --enable-extended-const --enable-all --verbose --quiet --log-format --log-events --color --help <FILE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --profiler)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-debug-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-num-threads)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__wast)
            opts="-f -v -q -h --singlepass --cranelift --llvm --v8 --wamr --wasmi --enable-verifier --profiler --llvm-debug-dir --enable-pass-params-opt --llvm-num-threads --enable-simd --disable-threads --enable-threads --enable-reference-types --enable-multi-value --enable-bulk-memory --enable-tail-call --enable-module-linking --enable-multi-memory --enable-memory64 --enable-exceptions --enable-relaxed-simd --enable-extended-const --enable-all --fail-fast --hash-algorithm --verbose --quiet --log-format --log-events --color --help <FILE>"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --profiler)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-debug-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --llvm-num-threads)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --hash-algorithm)
                    COMPREPLY=($(compgen -W "sha256 xx-hash" -- "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
        wasmer__whoami)
            opts="-v -q -h --wasmer-dir --cache-dir --registry --token --verbose --quiet --log-format --log-events --color --help"
            if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]] ; then
                COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
                return 0
            fi
            case "${prev}" in
                --wasmer-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --cache-dir)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --registry)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --token)
                    COMPREPLY=($(compgen -f "${cur}"))
                    return 0
                    ;;
                --log-format)
                    COMPREPLY=($(compgen -W "text json" -- "${cur}"))
                    return 0
                    ;;
                --log-events)
                    COMPREPLY=($(compgen -W "new close all" -- "${cur}"))
                    return 0
                    ;;
                --color)
                    COMPREPLY=($(compgen -W "auto always never" -- "${cur}"))
                    return 0
                    ;;
                *)
                    COMPREPLY=()
                    ;;
            esac
            COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
            return 0
            ;;
    esac
}

if [[ "${BASH_VERSINFO[0]}" -eq 4 && "${BASH_VERSINFO[1]}" -ge 4 || "${BASH_VERSINFO[0]}" -gt 4 ]]; then
    complete -F _wasmer -o nosort -o bashdefault -o default wasmer
else
    complete -F _wasmer -o bashdefault -o default wasmer
fi
