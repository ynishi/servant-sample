# servant-sample
* Very thanks to https://github.com/lotz84/ac2015-servant-example . This sample inspired by it. 
* Based on servant stack-template
* Separate Entity, Usecase and Framework.

## usage
```
stack build
stack exec servant-sample
```
## sample
* post
```
curl -vvv -H "Accept: application/json" -H "Content-type: application/json" -X POST 'localhost:8080/todos' -d '{"tag": "Req", "contents":"task1"}'
```
* get
```
curl -vvv "localhost:8080/todos/all"
```
