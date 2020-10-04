echo "1. Display calender of current month"
echo "2. Display calender of next 3 month"
echo "3. Display today's month"
echo "4. Display date in the format MM/DD/YYYY "
echo "5. Display full name of present month"
echo "enter choice:"
choice=$1
case $choice in 
1)
	echo $(cal)
	;;
2)
	echo $(ncal 3)
	;;
3)
	echo $(date +%d)
	;;
4)
	echo $(date +%D)
	;;
5)
	echo $(date +%B)
	;;

*)
	echo "wrong input"
	;;
esac
 
