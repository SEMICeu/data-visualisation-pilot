package organisationsNetwork;

import java.io.IOException;

public class Main {
	public static void main(String[] args) throws IOException {
		
		if(args.length != 2) {
			System.out.println("This program needs two arguments: input folder and output folder");
			System.out.println("Please run the program with the following command:");
			System.out.println("> jarname.jar inputFolder outputFolder");
		}
		else{
			System.out.println("Starting...");
			buildGEXF a = new buildGEXF();
			a.script(args[0], args[1]);
		}
	}
}
