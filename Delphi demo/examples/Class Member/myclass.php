<?php

/**
 * Class myclass let you store and print a full name.
 */
class myclass {
    /** First name */
    public $firstname;
    /** Last name */
    public $lastname;

    /**
     * @param string $firstname Sets the first name.
     * @param string $lastname Sets the last name.
     */
    function set_name($first, $last) {
        $this->firstname = $first;
        $this->lastname = $last;
    }
    /** Print the full name stored in the class. */
    function print() {
        echo $this->firstname . " " . $this->lastname . "\n";
    }
}

$obj = new myclass;
$obj->set_name("Donald", "Duck");
$obj->print();

$obj->firstname = "Micky";
$obj->lastname = "Mouse";
$obj->print();

?>