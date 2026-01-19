#Create a Python file in a code editor and write an "answering machine" program.
#Inform the user who they're trying to reach.
#Request the user's name, a contact method, and the reason for contacting you. Store these in separate variables.
#Display the user's information on the screen.
#(Bonus) Instead of displaying the user's information, write it to a file. Previous records should not be erased when running the program again.


print('Enter name:', end= ' ')
name = input()
print('Enter contact information:', end= ' ')
contact = input()
print('Enter reason for contact:', end= ' ')
reason = input()
print('The following information was received:')
print('The person who called was', name)
print('Here is the contact information', contact)
print('The reason was', reason)
