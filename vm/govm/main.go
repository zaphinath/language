package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"language/vm/govm/vm"
)

func main() {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println(r)
		}
	}()

	byts, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		log.Fatal(err.Error())
	}

	err = vm.NewVM(byts).Run()
	if err != nil {
		log.Fatal(err.Error())
	}
}
