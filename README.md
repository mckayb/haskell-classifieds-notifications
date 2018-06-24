# Haskell Classifieds Notifications

Crawls popular sites for certain search terms and notifies you of any new listings matching those terms.

## Usage
You'll need to install Docker and Haskell Stack.
Once those are installed, create your env file with the proper variables.

You'll need an account on Sendgrid and an AT&T phone number, at least until I get other providers working, or rework the notification system.
```
cp env.example .env
```

Then, use stack build the project.
```
stack build --copy-bins
```

Finally, build and run the docker image, referencing the .env file.
```
docker build -t hcn .
```
```
docker run --name hcn --rm --env-file .env hcn
```